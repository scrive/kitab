module Driver where

import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString qualified as Console
import Effectful.Environment (Environment)
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import KDL qualified
import Layoutz
import Optics.Core
import System.OsPath qualified as OsPath
import Validation

import CLI.Error
import CLI.Types
import Core.Graph
import Core.Model.ContextName
import Core.Model.Inventory.Aggregated
import Core.Model.Inventory.Selector (Selector (..))
import Core.Model.InventoryVariable
import Core.Model.Service
import Core.Model.ServiceContext
import Core.Validation
import Driver.Cilium (renderToCilium)
import Driver.Colours (TerminalColoursSettings (..), computeTerminalColoursSettings, stylise)
import Driver.Environment (EnvVars (..), getEnvironment)
import Driver.Inventory
import Driver.Puml (renderToPuml)
import Driver.Variable
import Driver.Verbosity
import Parser
import Parser.Types

runOptions
  :: (Console :> es, FileSystem :> es, Error (NonEmpty CLIError) :> es, Environment :> es)
  => Options
  -> Eff es ()
runOptions options = do
  environment <- getEnvironment
  coloursSettings <- computeTerminalColoursSettings environment.noColors
  let verbosity = computeVerbosity options.quiet environment.debug
  outputDirectory <- OsPath.decodeUtf options.outputDir
  FileSystem.createDirectoryIfMissing True outputDirectory
  missingPaths <- concatForM options.inputs $ \path -> do
    filePath <- OsPath.decodeUtf path
    fileDoesExist <- FileSystem.doesFileExist filePath
    if fileDoesExist
      then pure []
      else pure [path]
  case NE.nonEmpty missingPaths of
    Just errors -> do
      Error.throwError (fmap noFileAtProvidedLocation errors)
    Nothing -> do
      declarations <- concatForM options.inputs $ \inputPath -> do
        fileContent <- FileSystem.readFile =<< OsPath.decodeUtf inputPath
        let result = KDL.decodeWith decodeServiceDocument $ T.decodeUtf8 fileContent
        case result of
          Right a -> pure a
          Left err -> Error.throwError . NE.singleton $ kdlParseError inputPath err
      let contexts =
            mapMaybe
              ( \case
                  ContextDeclaration c -> Just c
                  _ -> Nothing
              )
              declarations
      let cloudSelector = Selector options.cloud
      let regionSelector = Selector options.region
      let envSelector = Selector options.environment
      inventories <- concat <$> traverse getInventories options.inventory
      let aggregatedInventory =
            mergeInventories
              cloudSelector
              regionSelector
              envSelector
              inventories
      let entities =
            mapMaybe
              ( \case
                  EntityDeclaration e -> Just e
                  _ -> Nothing
              )
              declarations
      serviceDefinitions <- do
        declarations
          & mapMaybe
            ( \case
                ServiceDeclaration s -> Just s
                _ -> Nothing
            )
          & traverse (resolveServiceVars aggregatedInventory)

      let graph = buildGraph serviceDefinitions entities
      let entitiesIndex = buildEntityIndex entities
      let serviceIndex = buildServiceIndex serviceDefinitions
      when (isVerbose verbosity) $ printInventory coloursSettings aggregatedInventory
      case checkGraph graph of
        Failure errors -> Error.throwError $ fmap graphValidationError errors
        Success _ -> do
          case options.format of
            PumlFormat ->
              renderToPuml
                serviceIndex
                contexts
                options.outputDir
                verbosity
                graph
            CiliumFormat ->
              renderToCilium
                serviceIndex
                entitiesIndex
                options.outputDir
                verbosity
                serviceDefinitions

filterServicesByContext
  :: Error (NonEmpty CLIError) :> es
  => List ContextName
  -> List ServiceContext
  -> List (Service var)
  -> Eff es (List (Service var))
filterServicesByContext [] _ services = pure services
filterServicesByContext contextFilters contexts services = do
  filteredServices <- forM contextFilters $ \contextFilter -> filterServices contextFilter
  pure $ List.concat filteredServices
  where
    filterServices contextFilter = do
      unless (List.any (\c -> c.contextName == contextFilter) contexts) $ Error.throwError (NE.singleton (unknownContextFilter contextFilter))
      pure $
        List.filter (\s -> (s ^. #serviceInfo % #serviceContext :: Maybe ContextName) == Just contextFilter) services

printInventory :: Console :> es => TerminalColoursSettings -> AggregatedInventory -> Eff es ()
printInventory coloursSettings AggregatedInventory {aggregatedAttributes, aggregatedVars} = do
  let tableElements =
        aggregatedVars
          & Map.elems
          <&> (\InventoryVariable {name, value} -> (T.unpack (display name), T.unpack value))
  let getAttribute attr = T.unpack $ Map.findWithDefault "N/A" attr aggregatedAttributes
  let inventoryTable =
        layout
          [ text "╭─ Inventory ───────────────────────────────────────"
          , margin
              "│"
              [ stylise coloursSettings StyleBold $ text "Attributes:"
              , margin
                  "  "
                  [kv [("Cloud", getAttribute "cloud"), ("Region", getAttribute "region"), ("Environment", getAttribute "env")]]
              ]
          , margin
              "│"
              [ stylise coloursSettings StyleBold $ text "Variables:"
              , margin
                  "  "
                  [kv tableElements]
              ]
          , text "╰───────────────────────────────────────────────────"
          ]

  Console.putStrLn (T.encodeUtf8 (T.pack (render inventoryTable)))

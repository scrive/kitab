module CLI.Cmd.Generate where

import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Effectful
import Effectful.Console.ByteString
import Effectful.Console.ByteString qualified as Console
import Effectful.Environment
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem
import Effectful.FileSystem qualified as FileSystem
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import Layoutz
import Optics.Core
import System.OsPath
import System.OsPath qualified as OsPath
import Validation

import CLI.Error
import Core.Graph
import Core.Model.ContextName
import Core.Model.Inventory.Aggregated
import Core.Model.Inventory.Selector (Selector (..))
import Core.Model.InventoryVariable
import Core.Model.Service
import Core.Model.ServiceContext
import Core.Validation
import Driver.Cilium
import Driver.Colours
import Driver.Environment
import Driver.Environment qualified as Driver
import Driver.GEXF
import Driver.Inventory
import Driver.Puml
import Driver.Variable
import Driver.Verbosity (computeVerbosity, isVerbose)
import Parser (parseKitabDocument)
import Parser.V1.Types

data GenerateOptions = GenerateOptions
  { quiet :: Bool
  , format :: OutputFormat
  , outputDir :: OsPath
  , contextFilters :: List ContextName
  , cloud :: Maybe Text
  , region :: Maybe Text
  , environment :: Maybe Text
  , inventory :: Maybe OsPath
  , inputs :: List OsPath
  }
  deriving stock (Eq, Ord, Show)

data OutputFormat
  = PumlFormat
  | CiliumFormat
  | GexfFormat
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Display OutputFormat where
  displayBuilder = \case
    PumlFormat -> "puml"
    CiliumFormat -> "cilium"
    GexfFormat -> "gexf"

runGenerate
  :: (Console :> es, FileSystem :> es, Error (NonEmpty CLIError) :> es, Environment :> es)
  => GenerateOptions
  -> Eff es ()
runGenerate options = do
  environment <- Driver.getEnvironment
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
        filePath <- OsPath.decodeUtf inputPath
        fileContent <- FileSystem.readFile filePath
        let result = parseKitabDocument filePath (T.decodeUtf8 fileContent)
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

      cidrDefinitions <-
        mapMaybe
          ( \case
              CIDRSetDeclaration c -> Just c
              _ -> Nothing
          )
          declarations
          & traverse (resolveCIDRVars aggregatedInventory)
      let graph = buildGraph serviceDefinitions entities
      let entitiesIndex = buildEntityIndex entities
      let serviceIndex = buildServiceIndex serviceDefinitions
      let cidrIndex = buildCidrIndex cidrDefinitions
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
                cidrIndex
                options.outputDir
                verbosity
                serviceDefinitions
            GexfFormat ->
              renderGEXF
                serviceIndex
                options.outputDir
                verbosity
                graph

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

  Console.putStrLn (T.encodeUtf8 (renderText inventoryTable))

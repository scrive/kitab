module CLI.Cmd.Generate
  ( GenerateOptions (..)
  , OutputFormat (..)
  , runGenerate
  , supportedOutputFormats
  ) where

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
import System.OsPath
import System.OsPath qualified as OsPath
import Validation

import CLI.Error
import Core.Graph
import Core.Model.ContextName
import Core.Model.Inventory.Aggregated
import Core.Model.Inventory.Selector (Selector (..))
import Core.Model.InventoryVariable
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

supportedOutputFormats :: Map Text OutputFormat
supportedOutputFormats =
  [minBound .. maxBound]
    <&> (\connType -> (display connType, connType))
    & Map.fromList

runGenerate
  :: (Console :> es, FileSystem :> es, Error (NonEmpty CLIError) :> es, Environment :> es)
  => GenerateOptions
  -> Eff es Unit
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
      allDeclarations <- concatForM options.inputs $ \inputPath -> do
        filePath <- OsPath.decodeUtf inputPath
        fileContent <- FileSystem.readFile filePath
        let result = parseKitabDocument filePath (T.decodeUtf8 fileContent)
        case result of
          Right a -> pure a
          Left err -> Error.throwError . NE.singleton $ kdlParseError inputPath err
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
      let declarations = partitionDeclarations allDeclarations
      let entities = declarations.entities
      let contexts = declarations.contexts
      services <- traverse (resolveServiceVars aggregatedInventory) declarations.services
      cidrs <- traverse (resolveCIDRVars aggregatedInventory) declarations.cidrs
      let graph = buildGraph services entities
      let entitiesIndex = buildEntityIndex entities
      let serviceIndex = buildServiceIndex services
      let cidrIndex = buildCidrIndex cidrs
      when (isVerbose verbosity) $ printInventory coloursSettings aggregatedInventory
      case checkGraph graph of
        Failure errors -> Error.throwError $ fmap graphValidationError errors
        Success _ -> do
          case options.format of
            PumlFormat ->
              renderToPuml
                serviceIndex
                options.outputDir
                verbosity
                graph
            CiliumFormat ->
              renderToCilium
                options.contextFilters
                contexts
                serviceIndex
                entitiesIndex
                cidrIndex
                options.outputDir
                verbosity
                services
            GexfFormat ->
              renderGEXF
                serviceIndex
                options.outputDir
                verbosity
                graph

printInventory :: Console :> es => TerminalColoursSettings -> AggregatedInventory -> Eff es Unit
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

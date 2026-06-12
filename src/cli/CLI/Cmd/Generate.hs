module CLI.Cmd.Generate
  ( GenerateOptions (..)
  , OutputFormat (..)
  , runGenerate
  , supportedOutputFormats
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Environment
import Effectful.Error.Static (Error)
import Effectful.FileSystem
import Effectful.FileSystem qualified as FileSystem
import System.OsPath
import System.OsPath qualified as OsPath

import CLI.Error
import Core.Graph
import Core.Model.ContextName
import Core.Model.Inventory.Aggregated
import Core.Model.Inventory.Selector (Selector (..))
import Driver.Cilium
import Driver.Colours
import Driver.Environment qualified as Driver
import Driver.GEXF
import Driver.Inventory
import Driver.Parsing (parseInputs)
import Driver.Puml
import Driver.Variable
import Driver.Verbosity (computeVerbosity, isVerbose)
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
  allDeclarations <- partitionDeclarations <$> parseInputs options.inputs
  inventories <- concat <$> traverse getInventories options.inventory
  let aggregatedInventory =
        mergeInventories
          (Selector options.cloud)
          (Selector options.region)
          (Selector options.environment)
          inventories
  when (isVerbose verbosity) $ printInventory coloursSettings aggregatedInventory
  model <- prepareModel aggregatedInventory allDeclarations
  case options.format of
    PumlFormat -> renderToPuml model options.outputDir verbosity
    CiliumFormat -> renderToCilium options.contextFilters model options.outputDir verbosity
    GexfFormat -> renderToGEXF model options.outputDir verbosity

-- | 1. Resolve inventory variables of Services
--   2. Resolve inventory variables of CIDR Sets
--  assemble the render model, and validate its connection graph.
-- Accumulated 'CLIError's are thrown on any unresolved variable or graph inconsistency,
-- so that the format-specific renderers downstream operate on a known-good model.
prepareModel
  :: Error (NonEmpty CLIError) :> es
  => AggregatedInventory
  -> Declarations
  -> Eff es PreparedModel
prepareModel aggregatedInventory declarations = do
  services <- traverse (resolveServiceVars aggregatedInventory) declarations.services
  cidrs <- traverse (resolveCIDRVars aggregatedInventory) declarations.cidrs
  let result = buildPreparedModel services declarations.entities cidrs declarations.contexts
  throwOnFailure graphValidationError result

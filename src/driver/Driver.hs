module Driver where

import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text.Encoding qualified as T
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import KDL qualified
import Optics.Core
import System.OsPath qualified as OsPath
import Validation

import CLI.Error
import CLI.Types
import Core.Graph
import Core.Model.ContextName
import Core.Model.Inventory
import Core.Model.Service
import Core.Model.ServiceContext
import Core.Validation
import Driver.Cilium (renderToCilium)
import Driver.Inventory
import Driver.Puml (renderToPuml)
import Driver.Variable
import Parser
import Parser.Types

runOptions
  :: (Console :> es, FileSystem :> es, Error (NonEmpty CLIError) :> es)
  => Options
  -> Eff es ()
runOptions options = do
  outputDirectory <- OsPath.decodeUtf options.outputDir
  FileSystem.createDirectoryIfMissing True outputDirectory
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
  inventories <- concat <$> traverse getInventories options.inventory
  let aggregatedInventory = mergeInventories inventories
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
  case checkGraph graph of
    Failure errors -> Error.throwError $ fmap graphValidationError errors
    Success _ -> do
      case options.format of
        PumlFormat ->
          renderToPuml
            serviceIndex
            contexts
            options.outputDir
            options.quiet
            graph
        CiliumFormat ->
          renderToCilium
            serviceIndex
            entitiesIndex
            options.outputDir
            options.quiet
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

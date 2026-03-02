{-# LANGUAGE QuasiQuotes #-}

module Driver where

import Algebra.Graph.Labelled qualified as Graph
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Control.DeepSeq
import Data.ByteString.Char8 qualified as BS8
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString qualified as Console
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import KDL qualified
import Optics.Core
import System.OsPath (osp, (</>))
import System.OsPath qualified as OsPath
import Validation

import CLI.Error
import CLI.Types
import Core.Filtering
import Core.Graph
import Core.Model.ContextName
import Core.Model.Service
import Core.Model.ServiceContext
import Core.Validation
import Core.VariableIndex
import Parser
import Parser.Types
import Render.C4 qualified as C4
import Render.C4.C4Service.Types qualified as C4
import Render.Cilium qualified as Cilium

runOptions :: (Console :> es, FileSystem :> es, Error (NonEmpty CLIError) :> es) => Options -> Eff es ()
runOptions options = do
  let filters = options.filters
  outputDirectory <- OsPath.decodeUtf options.outputDir
  FileSystem.createDirectoryIfMissing True outputDirectory
  declarations <- concatForM options.inputs $ \inputPath -> do
    fileContent <- FileSystem.readFile =<< OsPath.decodeUtf inputPath
    let result = KDL.decodeWith decodeServiceDocument $ T.decodeUtf8 fileContent
    case result of
      Right a -> pure a
      Left err -> Error.throwError . NE.singleton $ kdlParseError inputPath err
  let variables =
        mapMaybe
          ( \case
              VariableDeclaration v -> Just v
              _ -> Nothing
          )
          declarations
  let variableIndex = buildVariableIndex variables
  let contexts =
        mapMaybe
          ( \case
              ContextDeclaration c -> Just c
              _ -> Nothing
          )
          declarations
  let entities =
        mapMaybe
          ( \case
              EntityDeclaration e -> Just e
              _ -> Nothing
          )
          declarations
  let serviceDefinitions' =
        declarations
          & mapMaybe
            ( \case
                ServiceDeclaration s -> Just s
                _ -> Nothing
            )
          & concatMap (\s -> fmap (\f -> filterServiceOnAttributes f s) filters)

  let graph = buildGraph serviceDefinitions' entities
  let serviceIndex = buildServiceIndex serviceDefinitions'
  let entitiesIndex = buildEntityIndex entities
  serviceDefinitions <-
    filterServicesByContext
      options.contextFilters
      contexts
      serviceDefinitions'
  case checkGraph graph of
    Failure errors -> Error.throwError $ fmap graphValidationError errors
    Success _ -> do
      case options.format of
        PumlFormat -> do
          let graphEdges =
                graph
                  & Graph.edgeList
                  & fmap (\(es, a, b) -> (es, C4.toC4Service serviceIndex a, C4.toC4Service serviceIndex b))
          let adjacencyMap = AM.edges graphEdges
          let rendered = C4.renderC4 contexts adjacencyMap

          outputPath <- OsPath.decodeUtf (options.outputDir </> [osp|architecture.c4|])
          unless options.quiet (Console.putStrLn $ "Writing file " <> BS8.pack outputPath)
          FileSystem.writeFile outputPath (T.encodeUtf8 rendered)
        CiliumFormat -> do
          outputs <- forM serviceDefinitions $ \service -> do
            let rendered = Cilium.renderCilium (Cilium.toCiliumPolicy serviceIndex entitiesIndex service)
            outputFile <- OsPath.encodeUtf . T.unpack $ display service.serviceName
            outputPath <- OsPath.decodeUtf (options.outputDir </> outputFile <> [osp|-network-policy.yaml|])
            pure (outputPath, rendered)
          forM_ (force outputs) $ \(outputPath, rendered) -> do
            unless options.quiet (Console.putStrLn $ "Writing file " <> BS8.pack outputPath)
            FileSystem.writeFile outputPath (T.encodeUtf8 rendered)

filterServicesByContext
  :: Error (NonEmpty CLIError) :> es
  => List ContextName
  -> List ServiceContext
  -> List Service
  -> Eff es (List Service)
filterServicesByContext [] _ services = pure services
filterServicesByContext contextFilters contexts services = do
  filteredServices <- forM contextFilters $ \contextFilter -> filterServices contextFilter
  pure $ List.concat filteredServices
  where
    filterServices :: Error (NonEmpty CLIError) :> es => ContextName -> Eff es (List Service)
    filterServices contextFilter = do
      unless (List.any (\c -> c.contextName == contextFilter) contexts) $ Error.throwError (NE.singleton (unknownContextFilter contextFilter))
      pure $
        List.filter (\s -> (s ^. #serviceInfo % #serviceContext :: Maybe ContextName) == Just contextFilter) services

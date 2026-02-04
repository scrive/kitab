module Driver where

import Algebra.Graph.Labelled qualified as Graph
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text.Encoding qualified as T
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import KDL qualified
import System.OsPath qualified as OsPath
import Validation

import CLI.Error
import CLI.Types
import Core.Graph
import Core.Validation
import Parser
import Render.C4 qualified as C4
import Render.C4.Types qualified as C4
import Render.Cilium qualified as Cilium

runOptions :: (FileSystem :> es, Error (NonEmpty CLIError) :> es) => Options -> Eff es ()
runOptions options = do
  serviceDefinitions <- concatForM options.inputs $ \inputPath -> do
    fileContent <- FileSystem.readFile =<< OsPath.decodeUtf inputPath
    let result = KDL.decodeWith decodeServiceDocument $ T.decodeUtf8 fileContent
    case result of
      Right a -> pure a
      Left err -> Error.throwError . NE.singleton $ kdlParseError inputPath err
  let graph = buildGraph serviceDefinitions
  case checkGraph graph of
    Failure errors -> Error.throwError $ fmap graphValidationError errors
    Success _ -> do
      outputPath <- OsPath.decodeUtf options.output
      case options.format of
        PumlFormat -> do
          let graphEdges =
                graph
                  & Graph.edgeList
                  & fmap (\(es, a, b) -> (es, C4.toC4Service a, C4.toC4Service b))
          let adjacencyMap = AM.edges graphEdges
          let rendered = C4.renderC4 adjacencyMap

          FileSystem.writeFile outputPath (T.encodeUtf8 rendered)
        CiliumFormat -> do
          let serviceIndex = buildIndex serviceDefinitions
          forM_ serviceDefinitions $ \service -> do
            let rendered = Cilium.renderCilium (Cilium.toCiliumPolicy serviceIndex service)
            FileSystem.writeFile outputPath (T.encodeUtf8 rendered)

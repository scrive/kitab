{-# LANGUAGE QuasiQuotes #-}

module Driver.Puml where

import Algebra.Graph.Labelled (Graph)
import Algebra.Graph.Labelled qualified as Graph
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict qualified as Map
import Data.Text.Encoding qualified as T
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString qualified as Console
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import System.OsPath (OsPath, osp, (</>))
import System.OsPath qualified as OsPath

import Core.Model.Reference
import Core.Model.Service
import Core.Model.ServiceContext
import Core.Model.ServiceName
import Driver.Verbosity
import Render.C4 qualified as C4
import Render.C4.C4Service.Types qualified as C4

renderToPuml
  :: (Console :> es, FileSystem :> es)
  => Map ServiceName (ServiceInfo var)
  -> List ServiceContext
  -> OsPath
  -> VerbositySetting
  -> Graph (List ConnectionType) Reference
  -> Eff es ()
renderToPuml serviceIndex contexts outputDir verbosity graph = do
  let graphEdges =
        graph
          & Graph.edgeList
          & fmap (\(es, a, b) -> (es, C4.toC4Service serviceIndex a, C4.toC4Service serviceIndex b))
  let adjacencyMap = AM.edges graphEdges
  let toolCalls =
        graph
          & Graph.vertexList
          <&> \case
            ToolRef _ caller toolName -> Map.singleton caller [toolName]
            _ -> Map.empty
          & Map.unionsWith (++)
  let rendered = C4.renderC4 contexts adjacencyMap toolCalls
  outputPath <- OsPath.decodeUtf (outputDir </> [osp|architecture.c4|])
  when (isVerbose verbosity) (Console.putStrLn $ "Writing file " <> BS8.pack outputPath)
  FileSystem.writeFile outputPath (T.encodeUtf8 rendered)

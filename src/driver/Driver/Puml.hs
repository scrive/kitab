{-# LANGUAGE QuasiQuotes #-}

module Driver.Puml where

import Algebra.Graph.Labelled (Graph)
import Algebra.Graph.Labelled qualified as Graph
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.FileSystem (FileSystem)
import System.OsPath (OsPath, osp, (</>))
import System.OsPath qualified as OsPath

import Core.Model.Reference
import Core.Model.Service
import Core.Model.ServiceName
import Driver.Output (writeArtifact)
import Driver.Verbosity
import Render.C4 qualified as C4
import Render.C4.C4Container.Types qualified as C4

renderToPuml
  :: (Console :> es, FileSystem :> es)
  => Map ServiceName (ServiceInfo var)
  -> OsPath
  -> VerbositySetting
  -> Graph (List ConnectionType) Reference
  -> Eff es Unit
renderToPuml serviceIndex outputDir verbosity graph = do
  let graphEdges =
        graph
          & Graph.edgeList
          & fmap (\(es, a, b) -> (es, C4.toC4Container serviceIndex a, C4.toC4Container serviceIndex b))
  let adjacencyMap = AM.edges graphEdges
  let rendered = C4.renderC4 adjacencyMap
  outputPath <- OsPath.decodeUtf (outputDir </> [osp|architecture.c4|])
  writeArtifact verbosity outputPath rendered

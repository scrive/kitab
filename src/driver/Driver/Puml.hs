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
import Render.Puml qualified as Puml
import Render.Puml.C4Container.Types qualified as Puml

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
          & fmap (\(es, a, b) -> (es, Puml.toC4Container serviceIndex a, Puml.toC4Container serviceIndex b))
  let adjacencyMap = AM.edges graphEdges
  let rendered = Puml.renderPuml adjacencyMap
  outputPath <- OsPath.decodeUtf (outputDir </> [osp|architecture.puml|])
  writeArtifact verbosity outputPath rendered

{-# LANGUAGE QuasiQuotes #-}

module Driver.GEXF where

import Algebra.Graph.Labelled (Graph)
import Algebra.Graph.Labelled qualified as Graph
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Data.Void
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
import Render.GEXF qualified as Render

renderToGEXF
  :: (Console :> es, FileSystem :> es)
  => Map ServiceName (ServiceInfo Void)
  -> OsPath
  -> VerbositySetting
  -> Graph (List ConnectionType) Reference
  -> Eff es Unit
renderToGEXF serviceIndex outputDir verbosity graph = do
  let adjacencyMap =
        graph
          & Graph.edgeList
          & AM.edges
  let rendered = Render.renderToGEXF adjacencyMap serviceIndex
  outputPath <- OsPath.decodeUtf (outputDir </> [osp|architecture.gexf|])
  writeArtifact verbosity outputPath rendered

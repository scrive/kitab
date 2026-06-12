{-# LANGUAGE QuasiQuotes #-}

module Driver.GEXF where

import Algebra.Graph.Labelled qualified as Graph
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.FileSystem (FileSystem)
import System.OsPath (OsPath, osp, (</>))
import System.OsPath qualified as OsPath

import Core.Graph (PreparedModel (..))
import Driver.Output (writeArtifact)
import Driver.Verbosity
import Render.GEXF qualified as Render

renderToGEXF
  :: (Console :> es, FileSystem :> es)
  => PreparedModel
  -> OsPath
  -> VerbositySetting
  -> Eff es Unit
renderToGEXF model outputDir verbosity = do
  let adjacencyMap =
        model.graph
          & Graph.edgeList
          & AM.edges
  let rendered = Render.renderToGEXF adjacencyMap model.serviceIndex
  outputPath <- OsPath.decodeUtf (outputDir </> [osp|architecture.gexf|])
  writeArtifact verbosity outputPath rendered

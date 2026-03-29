{-# LANGUAGE QuasiQuotes #-}

module Driver.GEXF where

import Algebra.Graph.Labelled (Graph)
import Algebra.Graph.Labelled qualified as Graph
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Data.ByteString.Char8 qualified as BS8
import Data.Text.Encoding qualified as T
import Data.Void
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString qualified as Console
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import System.OsPath (OsPath, osp, (</>))
import System.OsPath qualified as OsPath

import Core.Model.Reference
import Core.Model.Service
import Core.Model.ServiceName
import Driver.Verbosity
import Render.GEXF qualified as Render

renderGEXF
  :: (Console :> es, FileSystem :> es)
  => Map ServiceName (ServiceInfo Void)
  -> OsPath
  -> VerbositySetting
  -> Graph (List ConnectionType) Reference
  -> Eff es ()
renderGEXF serviceIndex outputDir verbosity graph = do
  let adjacencyMap =
        graph
          & Graph.edgeList
          & AM.edges
  let rendered = Render.renderGEXF adjacencyMap serviceIndex
  outputPath <- OsPath.decodeUtf (outputDir </> [osp|architecture.gexf|])
  when (isVerbose verbosity) (Console.putStrLn $ "Writing file " <> BS8.pack outputPath)
  FileSystem.writeFile outputPath (T.encodeUtf8 rendered)

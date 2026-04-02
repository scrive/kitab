{-# LANGUAGE QuasiQuotes #-}

module Driver.Cilium where

import Control.DeepSeq
import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Void
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString qualified as Console
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import System.OsPath (osp, (</>))
import System.OsPath qualified as OsPath

import Core.Model.CIDRSet
import Core.Model.Entity
import Core.Model.EntityName
import Core.Model.Service
import Core.Model.ServiceName
import Driver.Verbosity
import Render.Cilium qualified as Cilium

renderToCilium
  :: (Console :> es, FileSystem :> es)
  => Map ServiceName (ServiceInfo Void)
  -> Map EntityName EntityInfo
  -> Map Text CIDRSet
  -> OsPath.OsPath
  -> VerbositySetting
  -> List (Service Void)
  -> Eff es ()
renderToCilium serviceIndex entitiesIndex cidrIndex outputDir verbosity serviceDefinitions = do
  outputs <- forM serviceDefinitions $ \service -> do
    let rendered = Cilium.renderCilium (Cilium.toCiliumPolicy serviceIndex entitiesIndex cidrIndex service)
    outputFile <- OsPath.encodeUtf . T.unpack $ display service.serviceName
    outputPath <- OsPath.decodeUtf (outputDir </> outputFile <> [osp|-network-policy.yaml|])
    pure (outputPath, rendered)
  forM_ (force outputs) $ \(outputPath, rendered) -> do
    when (isVerbose verbosity) (Console.putStrLn $ "Writing file " <> BS8.pack outputPath)
    FileSystem.writeFile outputPath (T.encodeUtf8 rendered)

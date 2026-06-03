{-# LANGUAGE QuasiQuotes #-}

module Driver.Cilium where

import Control.DeepSeq (force)
import Data.ByteString.Char8 qualified as BS8
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Void
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString qualified as Console
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import System.OsPath (osp, (</>))
import System.OsPath qualified as OsPath
import Validation

import CLI.Error
import Core.Model.CIDRSet
import Core.Model.ContextName
import Core.Model.Entity
import Core.Model.EntityName
import Core.Model.Service
import Core.Model.ServiceName
import Driver.Verbosity
import Render.Cilium qualified as Cilium
import Render.Cilium.Resolved

renderToCilium
  :: (Console :> es, Error (NonEmpty CLIError) :> es, FileSystem :> es)
  => List ContextName
  -> List ContextName
  -> Map ServiceName (ServiceInfo Void)
  -> Map EntityName EntityInfo
  -> Map Text (CIDRSet Void)
  -> OsPath.OsPath
  -> VerbositySetting
  -> List (Service Void)
  -> Eff es Unit
renderToCilium contextFilters knownContexts serviceIndex entitiesIndex cidrIndex outputDir verbosity serviceDefinitions = do
  validateContextFilters knownContexts contextFilters
  resolvedDefinitions <- resolveServices serviceIndex entitiesIndex cidrIndex serviceDefinitions
  let filteredDefinitions =
        case contextFilters of
          [] -> resolvedDefinitions
          _ -> List.filter (isInContext contextFilters) resolvedDefinitions
  outputs <- forM filteredDefinitions $ \service -> do
    let rendered = Cilium.renderCilium (Cilium.toCiliumPolicy service)
    outputFile <- OsPath.encodeUtf . T.unpack $ display service.serviceName
    outputPath <- OsPath.decodeUtf (outputDir </> outputFile <> [osp|-network-policy.yaml|])
    pure (outputPath, rendered)
  forM_ (force outputs) $ \(outputPath, rendered) -> do
    when (isVerbose verbosity) (Console.putStrLn $ "Writing file " <> BS8.pack outputPath)
    FileSystem.writeFile outputPath (T.encodeUtf8 rendered)

-- | Resolve every service against the declaration indices (see
-- 'resolveService'), so that rendering cannot fail. All violations are
-- accumulated and reported together.
resolveServices
  :: Error (NonEmpty CLIError) :> es
  => Map ServiceName (ServiceInfo Void)
  -> Map EntityName EntityInfo
  -> Map Text (CIDRSet Void)
  -> List (Service Void)
  -> Eff es (List ResolvedService)
resolveServices serviceIndex entityIndex cidrIndex serviceDefinitions =
  case traverse (resolveService serviceIndex entityIndex cidrIndex) serviceDefinitions of
    Failure violations -> Error.throwError $ fmap resolutionError violations
    Success resolved -> pure resolved

-- | Ensure every context passed via @--context@ corresponds to a context
-- declared in the document. Unknown filters are accumulated and reported
-- together so a typo does not silently yield empty output.
validateContextFilters
  :: Error (NonEmpty CLIError) :> es
  => List ContextName
  -> List ContextName
  -> Eff es Unit
validateContextFilters knownContexts contextFilters =
  forM_ (NE.nonEmpty $ List.filter (`notElem` knownContexts) contextFilters) $
    Error.throwError . fmap unknownContextFilter

isInContext :: List ContextName -> ResolvedService -> Bool
isInContext contextFilters service =
  case service.serviceContext of
    Nothing -> False
    Just c -> c `elem` contextFilters

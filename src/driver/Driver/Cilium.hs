{-# LANGUAGE QuasiQuotes #-}

module Driver.Cilium where

import Control.DeepSeq (force)
import Data.ByteString.Char8 qualified as BS8
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
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

import CLI.Error
import Core.Model.CIDRSet
import Core.Model.ContextName
import Core.Model.Entity
import Core.Model.EntityName
import Core.Model.Service
import Core.Model.ServiceName
import Driver.Verbosity
import Render.Cilium qualified as Cilium

renderToCilium
  :: (Console :> es, Error (NonEmpty CLIError) :> es, FileSystem :> es)
  => Map ServiceName (ServiceInfo Void)
  -> Map EntityName EntityInfo
  -> Map Text (CIDRSet Void)
  -> OsPath.OsPath
  -> VerbositySetting
  -> List (Service Void)
  -> Eff es Unit
renderToCilium serviceIndex entitiesIndex cidrIndex outputDir verbosity serviceDefinitions = do
  validateConnections serviceIndex cidrIndex serviceDefinitions
  outputs <- forM serviceDefinitions $ \service -> do
    let rendered = Cilium.renderCilium (Cilium.toCiliumPolicy serviceIndex entitiesIndex cidrIndex service)
    outputFile <- OsPath.encodeUtf . T.unpack $ display service.serviceName
    outputPath <- OsPath.decodeUtf (outputDir </> outputFile <> [osp|-network-policy.yaml|])
    pure (outputPath, rendered)
  forM_ (force outputs) $ \(outputPath, rendered) -> do
    when (isVerbose verbosity) (Console.putStrLn $ "Writing file " <> BS8.pack outputPath)
    FileSystem.writeFile outputPath (T.encodeUtf8 rendered)

-- | Check that every connection can be rendered as a Cilium egress rule:
-- service targets must be declared and reachable (per 'Cilium.reachability'),
-- and cidr-set targets must be declared. All violations are accumulated and
-- reported together.
validateConnections
  :: Error (NonEmpty CLIError) :> es
  => Map ServiceName (ServiceInfo Void)
  -> Map Text (CIDRSet Void)
  -> List (Service Void)
  -> Eff es Unit
validateConnections serviceIndex cidrIndex serviceDefinitions = do
  let serviceViolations =
        [ violation
        | service <- serviceDefinitions
        , Connection {connectionWith} <- service.serviceConnections
        , Just violation <-
            [checkConnection service.serviceInfo.serviceContext service.serviceName connectionWith]
        ]
  let cidrViolations =
        [ missingCidrSetTarget service.serviceName connection.connectTarget
        | service <- serviceDefinitions
        , connection <- service.cidrConnections
        , Map.notMember connection.connectTarget cidrIndex
        ]
  forM_ (NE.nonEmpty $ serviceViolations <> cidrViolations) Error.throwError
  where
    checkConnection :: Maybe ContextName -> ServiceName -> ServiceName -> Maybe CLIError
    checkConnection mContext source target =
      case Map.lookup target serviceIndex of
        Nothing -> Just $ missingConnectionTarget source target
        Just serviceInfo ->
          case Cilium.reachability mContext serviceInfo of
            Cilium.Unreachable -> Just $ unreachableConnectionTarget source target
            _ -> Nothing

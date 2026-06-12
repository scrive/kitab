module Parser.V1.Service
  ( serviceDecoder
  , toolDeclarationDecoder
  , ServiceMetadata (..)
  ) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC.Generics
import KDL

import Core.Model.ContextName
import Core.Model.PortNode
import Core.Model.Service
import Core.Variable
import Parser.Util (pickAll, pickOne)
import Parser.V1.EntityName
import Parser.V1.PortNode
import Parser.V1.ServiceContext
import Parser.V1.ServiceName
import Parser.V1.Var

data ServiceMetadata (var :: Type)
  = FQDNNode (Either var Text)
  | DependsOnNode Connection
  | ServiceContextNode ContextName
  | ServicePortNode PortNode
  | AccessNode EntityAccess
  | ConnectNode CIDRConnection
  | CallTool Text
  deriving stock (Eq, Ord, Show, Generic)

serviceDecoder :: NodeListDecoder (Service Var)
serviceDecoder = KDL.nodeWith "service" $ do
  serviceName <- KDL.argWith serviceNameDecoder
  rendererProps <- KDL.remainingProps
  mixedChildren <-
    KDL.children . KDL.many $
      (FQDNNode <$> fqdnDecoder)
        <|> (ServicePortNode <$> portDecoder)
        <|> (DependsOnNode <$> dependsOnDecoder)
        <|> (AccessNode <$> accessDecoder)
        <|> (ConnectNode <$> connectDecoder)
        <|> (ServiceContextNode <$> contextReferenceDecoder)
        <|> (CallTool <$> toolCallDecoder)

  let serviceFqdn = pickOne #_FQDNNode mixedChildren
  let servicePorts = Set.fromList $ pickAll #_ServicePortNode mixedChildren
  let serviceContext = pickOne #_ServiceContextNode mixedChildren
  let serviceInfo = ServiceInfo {serviceFqdn, serviceContext, servicePorts, rendererProps}
  let serviceConnections = pickAll #_DependsOnNode mixedChildren
  let entityAccesses = pickAll #_AccessNode mixedChildren
  let cidrConnections = pickAll #_ConnectNode mixedChildren
  let toolCalls = pickAll #_CallTool mixedChildren

  pure Service {serviceName, serviceInfo, serviceConnections, entityAccesses, cidrConnections, toolCalls}

dependsOnDecoder :: NodeListDecoder Connection
dependsOnDecoder = KDL.nodeWith "depends-on" $ do
  connectionWith <- KDL.argWith serviceNameDecoder
  (connectionPorts, connectionType) <- KDL.children $ do
    connectionPorts <- Set.fromList <$> KDL.many portDecoder
    connectionType <- KDL.nodeWith "via" connectionTypeDecoder
    pure (connectionPorts, connectionType)
  pure Connection {connectionWith, connectionType, connectionPorts}

accessDecoder :: NodeListDecoder EntityAccess
accessDecoder = KDL.nodeWith "access" $ do
  accessTarget <- KDL.argWith entityNameDecoder
  accessPorts <-
    KDL.children $
      Set.fromList <$> KDL.many portDecoder
  pure EntityAccess {accessTarget, accessPorts}

connectDecoder :: NodeListDecoder CIDRConnection
connectDecoder = KDL.nodeWith "connect" $ do
  connectTarget <- KDL.argWith KDL.string
  pure CIDRConnection {connectTarget}

connectionTypeDecoder :: NodeDecoder ConnectionType
connectionTypeDecoder = do
  connTypeText <- arg
  case Map.lookup connTypeText connectionTypes of
    Just r -> pure r
    Nothing -> KDL.fail $ "Found unknown connection type: " <> connTypeText <> ". Supported connection types are " <> mconcat (List.intersperse ", " supportedConnectionTypes)

supportedConnectionTypes :: List Text
supportedConnectionTypes = display <$> ([minBound .. maxBound] :: (List ConnectionType))

fqdnDecoder :: NodeListDecoder (Either Var Text)
fqdnDecoder = KDL.nodeWith "fqdn" varOrTextArg

toolCallDecoder :: NodeListDecoder Text
toolCallDecoder = KDL.nodeWith "call" $ do
  KDL.argWith KDL.string

toolDeclarationDecoder :: NodeListDecoder Text
toolDeclarationDecoder = KDL.nodeWith "tool" $ do
  KDL.argWith KDL.string

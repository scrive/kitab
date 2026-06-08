module Parser.V1.Service
  ( serviceDecoder
  , toolDeclarationDecoder
  , ServiceMetadata (..)
  ) where

import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import KDL

import Core.Model.ContextName
import Core.Model.PortNode
import Core.Model.Service
import Core.Variable
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
  deriving stock (Eq, Ord, Show)

getFQDN :: ServiceMetadata var -> Maybe (Either var Text)
getFQDN (FQDNNode t) = Just t
getFQDN _ = Nothing

getServiceContext :: ServiceMetadata var -> Maybe ContextName
getServiceContext (ServiceContextNode c) = Just c
getServiceContext _ = Nothing

getConnection :: ServiceMetadata var -> Maybe Connection
getConnection (DependsOnNode c) = Just c
getConnection _ = Nothing

getPort :: ServiceMetadata var -> Maybe PortNode
getPort (ServicePortNode p) = Just p
getPort _ = Nothing

getEntityAccess :: ServiceMetadata var -> Maybe EntityAccess
getEntityAccess (AccessNode a) = Just a
getEntityAccess _ = Nothing

getCidrConnection :: ServiceMetadata var -> Maybe CIDRConnection
getCidrConnection (ConnectNode a) = Just a
getCidrConnection _ = Nothing

getToolCall :: ServiceMetadata var -> Maybe Text
getToolCall (CallTool t) = Just t
getToolCall _ = Nothing

serviceDecoder :: NodeListDecoder (Service Var)
serviceDecoder = KDL.nodeWith "service" $ do
  serviceName <- KDL.argWith serviceNameDecoder
  mixedChildren <-
    KDL.children . KDL.many $
      (FQDNNode <$> fqdnDecoder)
        <|> (ServicePortNode <$> portDecoder)
        <|> (DependsOnNode <$> dependsOnDecoder)
        <|> (AccessNode <$> accessDecoder)
        <|> (ConnectNode <$> connectDecoder)
        <|> (ServiceContextNode <$> contextReferenceDecoder)
        <|> (CallTool <$> toolCallDecoder)

  let serviceFqdn = Maybe.listToMaybe $ mapMaybe getFQDN mixedChildren
  let servicePorts = Set.fromList $ mapMaybe getPort mixedChildren
  let serviceContext = Maybe.listToMaybe $ mapMaybe getServiceContext mixedChildren
  let serviceInfo = ServiceInfo {serviceFqdn, serviceContext, servicePorts}
  let serviceConnections = mapMaybe getConnection mixedChildren
  let entityAccesses = mapMaybe getEntityAccess mixedChildren
  let cidrConnections = mapMaybe getCidrConnection mixedChildren
  let toolCalls = mapMaybe getToolCall mixedChildren

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
  case connTypeText of
    "https" -> pure HTTPS
    "function-call" -> pure FunctionCall
    "smtps" -> pure SMTPS
    "redis" -> pure Redis
    "postgres" -> pure Postgres
    "domain" -> pure Domain
    "external-tool" -> pure ExternalTool
    "browser" -> pure Browser
    _ -> KDL.fail $ "Found unknown connection type: " <> connTypeText <> ". Supported connection types are " <> mconcat (List.intersperse ", " supportedConnectionTypes)

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

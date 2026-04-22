module Parser.V1.Service
  ( serviceDecoder
  , ServiceMetadata (..)
  ) where

import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import KDL

import Core.Model.ContextName
import Core.Model.InventoryVariable (VariableName (..))
import Core.Model.PortNode
import Core.Model.Service
import Core.Variable
import Parser.V1.EntityName
import Parser.V1.PortNode
import Parser.V1.ServiceContext
import Parser.V1.ServiceName

data ServiceMetadata (var :: Type)
  = FQDNNode (Either var Text)
  | DependsOnNode Connection
  | ServiceContextNode ContextName
  | ServicePortNode PortNode
  | AccessNode EntityAccess
  | ConnectNode CIDRConnection
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

  let serviceFqdn = Maybe.listToMaybe $ mapMaybe getFQDN mixedChildren
  let servicePorts = Set.fromList $ mapMaybe getPort mixedChildren
  let serviceContext = Maybe.listToMaybe $ mapMaybe getServiceContext mixedChildren
  let serviceInfo = ServiceInfo {serviceFqdn, serviceContext, servicePorts}
  let serviceConnections = mapMaybe getConnection mixedChildren
  let entityAccesses = mapMaybe getEntityAccess mixedChildren
  let cidrConnections = mapMaybe getCidrConnection mixedChildren

  pure Service {serviceName, serviceInfo, serviceConnections, entityAccesses, cidrConnections}

dependsOnDecoder :: NodeListDecoder Connection
dependsOnDecoder = KDL.nodeWith "depends-on" $ do
  connectionWith <- KDL.argWith serviceNameDecoder
  -- referenceName <- KDL.argWith serviceNameDecoder
  -- referenceContext <- KDL.optional $ KDL.propWith "context" (ContextName <$> KDL.string)
  -- pure referenceName
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
    _ -> KDL.fail $ "Found unknown connection type: " <> connTypeText

fqdnDecoder :: NodeListDecoder (Either Var Text)
fqdnDecoder =
  KDL.nodeWith "fqdn" $ do
    KDL.argWith' ["text", "var"] . KDL.withDecoder KDL.any $
      ( \val -> do
          s <- case val.data_ of
            KDL.String s -> pure s
            _ -> KDL.failM "Expected string"
          case (.identifier.value) <$> val.ann of
            Just "var" -> pure . Left $ Var (VariableName s)
            -- Nothing or Just "text"
            _ -> pure . Right $ s
      )

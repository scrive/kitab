module Parser.Service
  ( serviceDecoder
  , ServiceMetadata (..)
  ) where

import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import KDL

import Core.Model.CIDRSet
import Core.Model.ContextName
import Core.Model.InventoryVariable (VariableName (..))
import Core.Model.PortNode
import Core.Model.Service
import Core.Variable
import Parser.EntityName
import Parser.PortNode
import Parser.ServiceContext
import Parser.ServiceName

data ServiceMetadata (var :: Type)
  = FQDNNode (Either var Text)
  | DependsOnNode Connection
  | ServiceContextNode ContextName
  | CIDRSetNode CIDRSet
  | ServicePortNode PortNode
  | AccessNode EntityAccess
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

getCIDRSet :: ServiceMetadata var -> Maybe CIDRSet
getCIDRSet (CIDRSetNode c) = Just c
getCIDRSet _ = Nothing

getPort :: ServiceMetadata var -> Maybe PortNode
getPort (ServicePortNode p) = Just p
getPort _ = Nothing

getEntityAccess :: ServiceMetadata var -> Maybe EntityAccess
getEntityAccess (AccessNode a) = Just a
getEntityAccess _ = Nothing

serviceDecoder :: NodeListDecoder (Service Var)
serviceDecoder = KDL.nodeWith "service" $ do
  serviceName <- KDL.argWith serviceNameDecoder
  mixedChildren <-
    KDL.children . KDL.many $
      (FQDNNode <$> fqdnDecoder)
        <|> (ServicePortNode <$> portDecoder)
        <|> (DependsOnNode <$> dependsOnDecoder)
        <|> (AccessNode <$> accessDecoder)
        <|> (ServiceContextNode <$> contextReferenceDecoder)
        <|> (CIDRSetNode <$> cidrSetDecoder)

  let serviceFqdn = Maybe.listToMaybe $ mapMaybe getFQDN mixedChildren
  let servicePorts = Set.fromList $ mapMaybe getPort mixedChildren
  let serviceContext = Maybe.listToMaybe $ mapMaybe getServiceContext mixedChildren
  let serviceInfo = ServiceInfo {serviceFqdn, serviceContext, servicePorts}
  let serviceConnections = mapMaybe getConnection mixedChildren
  let cidrSets = mapMaybe getCIDRSet mixedChildren
  let entityAccesses = mapMaybe getEntityAccess mixedChildren

  pure Service {serviceName, serviceInfo, serviceConnections, cidrSets, entityAccesses}

dependsOnDecoder :: NodeListDecoder Connection
dependsOnDecoder = KDL.nodeWith "depends-on" $ do
  connectionWith <- KDL.argWith serviceNameDecoder
  -- referenceName <- KDL.argWith serviceNameDecoder
  -- referenceContext <- KDL.optional $ KDL.propWith "context" (ContextName <$> KDL.text)
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

connectionTypeDecoder :: NodeDecoder ConnectionType
connectionTypeDecoder = do
  connTypeText <- arg
  case connTypeText of
    "https" -> pure HTTPS
    "function-call" -> pure FunctionCall
    _ -> KDL.fail $ "Found unkonwn connection type: " <> connTypeText

cidrSetDecoder :: NodeListDecoder CIDRSet
cidrSetDecoder = KDL.nodeWith "cidr-set" $ do
  ports <- KDL.children $ KDL.many portDecoder
  items <-
    KDL.children $
      KDL.many
        ( cidrDecoder
            <|> exceptionDecoder
        )
  pure $ CIDRSet items ports

cidrDecoder :: NodeListDecoder CIDRSetItem
cidrDecoder = KDL.nodeWith "cidr" $ do
  cidr <- KDL.arg @Text
  name <- KDL.arg @Text
  pure $ CIDR cidr name

exceptionDecoder :: NodeListDecoder CIDRSetItem
exceptionDecoder = KDL.nodeWith "except" $ do
  cidr <- KDL.arg @Text
  reason <- KDL.arg @Text
  pure $ Except cidr reason

fqdnDecoder :: NodeListDecoder (Either Var Text)
fqdnDecoder =
  KDL.nodeWith "fqdn" $ do
    KDL.argWith' ["text", "var"] . KDL.withDecoder KDL.any $ (\val -> do
        s <- case val.data_ of
          KDL.String s -> pure s
          _ -> KDL.failM "Expected string"
        case (.identifier.value) <$> val.ann of
          Just "var" -> pure . Left $ Var (VariableName s)
          -- Nothing or Just "text"
          _ -> pure . Right $ s)

module Parser.Service where

import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import KDL
import KDL.Decoder.Internal.Decoder

import Core.Model.CIDRSet
import Core.Model.ContextName
import Core.Model.PortNode
import Core.Model.Service
import Parser.PortNode
import Parser.ServiceContext
import Parser.ServiceName

data ServiceMetadata
  = FQDNNode Text
  | DependsOnNode Connection
  | ServiceContextNode ContextName
  | CIDRSetNode CIDRSet
  | ServicePortNode PortNode
  deriving stock (Eq, Ord, Show)

getFQDN :: ServiceMetadata -> Maybe Text
getFQDN (FQDNNode t) = Just t
getFQDN _ = Nothing

getServiceContext :: ServiceMetadata -> Maybe ContextName
getServiceContext (ServiceContextNode c) = Just c
getServiceContext _ = Nothing

getConnection :: ServiceMetadata -> Maybe Connection
getConnection (DependsOnNode c) = Just c
getConnection _ = Nothing

getCIDRSet :: ServiceMetadata -> Maybe CIDRSet
getCIDRSet (CIDRSetNode c) = Just c
getCIDRSet _ = Nothing

getPort :: ServiceMetadata -> Maybe PortNode
getPort (ServicePortNode p) = Just p
getPort _ = Nothing

serviceDecoder :: DecodeArrow NodeList () Service
serviceDecoder = KDL.nodeWith "service" $ do
  serviceName <- KDL.argWith serviceNameDecoder
  mixedChildren <-
    KDL.children . KDL.many $
      (FQDNNode <$> KDL.nodeWith "fqdn" (KDL.argWith KDL.text))
        <|> (ServicePortNode <$> portDecoder)
        <|> (DependsOnNode <$> connectionDecoder)
        <|> (ServiceContextNode <$> contextReferenceDecoder)
        <|> (CIDRSetNode <$> cidrSetDecoder)

  let serviceFqdn = Maybe.listToMaybe $ mapMaybe getFQDN mixedChildren
  let servicePorts = Set.fromList $ mapMaybe getPort mixedChildren
  let serviceContext = Maybe.listToMaybe $ mapMaybe getServiceContext mixedChildren
  let serviceInfo = ServiceInfo {serviceFqdn, serviceContext, servicePorts}
  let connections = mapMaybe getConnection mixedChildren
  let cidrSets = mapMaybe getCIDRSet mixedChildren

  pure Service {serviceName, serviceInfo, connections, cidrSets}

connectionDecoder :: DecodeArrow NodeList () Connection
connectionDecoder = KDL.nodeWith "depends-on" $ do
  connectionWith <- do
    referenceName <- KDL.argWith serviceNameDecoder
    referenceContext <- KDL.argWith $ KDL.optional (ContextName <$> KDL.text)
    pure ServiceReference{referenceName, referenceContext}
  (connectionPorts, connectionType) <- KDL.children $ do
    connectionPorts <- Set.fromList <$> KDL.many portDecoder
    connectionType <- KDL.nodeWith "via" connectionTypeDecoder
    pure (connectionPorts, connectionType)
  pure Connection {connectionWith, connectionType, connectionPorts}

connectionTypeDecoder :: DecodeArrow Node () ConnectionType
connectionTypeDecoder = do
  connTypeText <- arg
  case connTypeText of
    "https" -> pure HTTPS
    "function-call" -> pure FunctionCall
    _ -> KDL.fail $ "Found unkonwn connection type: " <> connTypeText

cidrSetDecoder :: DecodeArrow NodeList () CIDRSet
cidrSetDecoder = KDL.nodeWith "cidr-set" $ do
  ports <- KDL.children $ KDL.many portDecoder
  items <-
    KDL.children $
      KDL.many
        ( cidrDecoder
            <|> exceptionDecoder
        )
  pure $ CIDRSet items ports

cidrDecoder :: DecodeArrow NodeList () CIDRSetItem
cidrDecoder = KDL.nodeWith "cidr" $ do
  cidr <- KDL.arg @Text
  name <- KDL.arg @Text
  pure $ CIDR cidr name

exceptionDecoder :: DecodeArrow NodeList () CIDRSetItem
exceptionDecoder = KDL.nodeWith "except" $ do
  cidr <- KDL.arg @Text
  reason <- KDL.arg @Text
  pure $ Except cidr reason

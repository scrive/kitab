module Parser.Service where

import Data.List qualified as List
import KDL
import KDL.Decoder.Internal.Decoder

import Core.Model.CIDRSet
import Core.Model.Service
import Core.Model.ServiceContext
import Parser.ServiceContext

data ServiceMetadata
  = FQDNNode Text
  | DependsOnNode Connection
  | ServiceContextNode ServiceContext
  | CIDRSetNode CIDRSet

getFQDN :: ServiceMetadata -> Maybe Text
getFQDN (FQDNNode t) = Just t
getFQDN _ = Nothing

getServiceContext :: ServiceMetadata -> Maybe ServiceContext
getServiceContext (ServiceContextNode c) = Just c
getServiceContext _ = Nothing

getConnection :: ServiceMetadata -> Maybe Connection
getConnection (DependsOnNode c) = Just c
getConnection _ = Nothing

getCIDRSet :: ServiceMetadata -> Maybe CIDRSet
getCIDRSet (CIDRSetNode c) = Just c
getCIDRSet _ = Nothing

serviceDecoder :: DecodeArrow Node () Service
serviceDecoder = do
  serviceName <- KDL.argWith serviceNameDecoder
  mixedChildren <-
    KDL.children . KDL.many $
      (FQDNNode <$> KDL.nodeWith "fqdn" (KDL.argWith KDL.text))
        <|> (DependsOnNode <$> KDL.nodeWith "depends-on" connectionDecoder)
        <|> (ServiceContextNode <$> KDL.nodeWith "context" contextDecoder)
        <|> (CIDRSetNode <$> KDL.nodeWith "cidr-set" cidrSetDecoder)

  let serviceFqdn = List.foldl' (\_ sm -> getFQDN sm) Nothing mixedChildren
  let serviceContext = List.foldl' (\_ sm -> getServiceContext sm) Nothing mixedChildren
  let serviceInfo = ServiceInfo {serviceFqdn, serviceContext}
  let connections = mapMaybe getConnection mixedChildren
  let cidrSets = mapMaybe getCIDRSet mixedChildren

  pure Service {serviceName, serviceInfo, connections, cidrSets}

connectionDecoder :: DecodeArrow Node () Connection
connectionDecoder = do
  connectionWith <- KDL.argWith serviceNameDecoder
  (connectionPort, connectionType) <- KDL.children $ do
    connectionPort <- optional $ KDL.nodeWith "port" arg
    connectionType <- KDL.nodeWith "via" connectionTypeDecoder
    pure (connectionPort, connectionType)
  pure Connection {connectionWith, connectionType, connectionPort}

connectionTypeDecoder :: DecodeArrow Node () ConnectionType
connectionTypeDecoder = do
  connTypeText <- arg
  case connTypeText of
    "https" -> pure HTTPS
    "function-call" -> pure FunctionCall
    _ -> KDL.fail $ "Found unkonwn connection type: " <> connTypeText

serviceNameDecoder :: ValueDecodeArrow () ServiceName
serviceNameDecoder = ServiceName <$> KDL.text

cidrSetDecoder :: DecodeArrow Node () CIDRSet
cidrSetDecoder = KDL.children $ do
  items <-
    KDL.many
      ( cidrDecoder
          <|> exceptionDecoder
      )
  pure $ CIDRSet items

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

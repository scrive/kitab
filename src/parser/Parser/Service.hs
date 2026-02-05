module Parser.Service where

import Data.List qualified as List
import KDL
import KDL.Decoder.Internal.Decoder

import Core.Model.Service
import Core.Model.ServiceContext
import Parser.ServiceContext

data ServiceMetadata
  = FQDN Text
  | DependsOn Connection
  | Context ServiceContext

getFQDN :: ServiceMetadata -> Maybe Text
getFQDN (FQDN t) = Just t
getFQDN _ = Nothing

getServiceContext :: ServiceMetadata -> Maybe ServiceContext
getServiceContext (Context c) = Just c
getServiceContext _ = Nothing

getConnection :: ServiceMetadata -> Maybe Connection
getConnection (DependsOn c) = Just c
getConnection _ = Nothing

serviceDecoder :: DecodeArrow Node () Service
serviceDecoder = do
  serviceName <- KDL.argWith serviceNameDecoder
  mixedChildren <-
    KDL.children . KDL.many $
      (FQDN <$> KDL.nodeWith "fqdn" (KDL.argWith KDL.text))
        <|> (DependsOn <$> KDL.nodeWith "depends-on" connectionDecoder)
        <|> (Context <$> KDL.nodeWith "context" contextDecoder)

  let serviceFqdn = List.foldl' (\_ sm -> getFQDN sm) Nothing mixedChildren
  let serviceContext = List.foldl' (\_ sm -> getServiceContext sm) Nothing mixedChildren

  let serviceInfo = ServiceInfo {serviceFqdn, serviceContext}
  let connections = mapMaybe getConnection mixedChildren

  pure Service {serviceName, serviceInfo, connections}

connectionDecoder :: DecodeArrow Node () Connection
connectionDecoder = do
  connectionWith <- KDL.argWith serviceNameDecoder
  connectionType <- KDL.argWith connectionTypeDecoder
  pure Connection {connectionWith, connectionType}

connectionTypeDecoder :: ValueDecodeArrow () ConnectionType
connectionTypeDecoder = do
  connTypeText <- KDL.text
  case connTypeText of
    "https" -> pure HTTPS
    "function-call" -> pure FunctionCall
    _ -> KDL.fail $ "Found unkonwn connection type: " <> connTypeText

serviceNameDecoder :: ValueDecodeArrow () ServiceName
serviceNameDecoder = ServiceName <$> KDL.text

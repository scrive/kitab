module Parser where

import KDL
import KDL.Decoder.Internal.Decoder

import Core.Model

decodeServiceDocument :: KDL.DocumentDecoder [Service]
decodeServiceDocument = KDL.document . KDL.many $ KDL.nodeWith "service" serviceDecoder

decodeService :: KDL.DocumentDecoder Service
decodeService = KDL.document $ do
  KDL.nodeWith "service" serviceDecoder

serviceDecoder :: DecodeArrow Node () Service
serviceDecoder = do
  serviceName <- KDL.argWith serviceNameDecoder
  connections <- KDL.children . many $ KDL.nodeWith "depends-on" connectionDecoder
  pure Service {serviceName, connections}

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

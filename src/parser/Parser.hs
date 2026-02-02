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
  -- Parse all children as either an FQDN (Left) or a Connection (Right).
  -- This allows out-of-order fields.
  mixedChildren <-
    KDL.children . KDL.many $
      (Left <$> KDL.nodeWith "fqdn" (KDL.argWith KDL.text))
        <|> (Right <$> KDL.nodeWith "depends-on" connectionDecoder)

  let serviceFqdn = listToMaybe (lefts mixedChildren)
  let serviceInfo = ServiceInfo {serviceFqdn}
  let connections = rights mixedChildren

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

module Parser.ServiceContext where

import KDL
import KDL.Decoder.Internal.Decoder

import Core.Model.ServiceContext

contextDecoder :: DecodeArrow NodeList () ServiceContext
contextDecoder = KDL.nodeWith "context" $ do
  contextName <- KDL.argWith contextNameDecoder
  pure ServiceContext {contextName}

contextNameDecoder :: ValueDecodeArrow () ContextName
contextNameDecoder = ContextName <$> KDL.text

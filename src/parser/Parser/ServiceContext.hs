module Parser.ServiceContext where

import KDL
import KDL.Decoder.Internal.Decoder

import Core.Model.ServiceContext

contextDecoder :: DecodeArrow Node () ServiceContext
contextDecoder = do
  contextName <- KDL.argWith contextNameDecoder
  pure ServiceContext {contextName}

contextNameDecoder :: ValueDecodeArrow () ContextName
contextNameDecoder = ContextName <$> KDL.text

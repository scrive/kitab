module Parser.ServiceContext where

import KDL
import KDL.Decoder.Internal.Decoder

import Core.Model.ContextName
import Core.Model.ServiceContext
import Parser.ContextEntity

contextDecoder :: DecodeArrow NodeList () ServiceContext
contextDecoder = KDL.nodeWith "context" $ do
  contextName <- KDL.argWith (ContextName <$> KDL.text)
  contextEntities <- KDL.children . KDL.many $ entityDecoder
  pure ServiceContext {contextName, contextEntities}

contextReferenceDecoder :: DecodeArrow NodeList () ContextName
contextReferenceDecoder =
  KDL.nodeWith "in-context" $
    ContextName <$> KDL.argWith KDL.text

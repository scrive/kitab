module Parser.ServiceContext where

import KDL
import KDL.Decoder.Internal.Decoder

import Core.Model.ContextName
import Core.Model.ServiceContext

contextDecoder :: DecodeArrow NodeList () ServiceContext
contextDecoder = KDL.nodeWith "context" $ do
  contextName <- KDL.argWith (ContextName <$> KDL.text)
  pure ServiceContext {contextName}

contextReferenceDecoder :: DecodeArrow NodeList () ContextName
contextReferenceDecoder =
  KDL.nodeWith "in-context" $
    ContextName <$> KDL.argWith KDL.text

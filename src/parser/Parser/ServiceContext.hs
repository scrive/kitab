module Parser.ServiceContext where

import KDL
import KDL.Decoder.Internal.Decoder

import Core.Model.ContextEntity
import Core.Model.ContextName
import Core.Model.ServiceContext

contextDecoder :: DecodeArrow NodeList () ServiceContext
contextDecoder = KDL.nodeWith "context" $ do
  contextName <- KDL.argWith (ContextName <$> KDL.text)
  -- Add entityDecoder here
  pure ServiceContext {contextName}

contextNameDecoder :: DecodeArrow NodeList () ContextName
contextNameDecoder =
  KDL.nodeWith "in" $
    ContextName <$> KDL.argWith KDL.text

module Parser.ServiceContext where

import KDL

import Core.Model.ContextName
import Core.Model.ServiceContext

contextDecoder :: NodeListDecoder ServiceContext
contextDecoder = KDL.nodeWith "context" $ do
  contextName <- KDL.argWith (ContextName <$> KDL.text)
  pure ServiceContext {contextName}

contextReferenceDecoder :: NodeListDecoder ContextName
contextReferenceDecoder =
  KDL.nodeWith "in-context" $
    ContextName <$> KDL.argWith KDL.text

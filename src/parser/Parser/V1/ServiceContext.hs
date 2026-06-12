module Parser.V1.ServiceContext where

import KDL

import Core.Model.ContextName
import Core.Model.ServiceContext

contextDecoder :: NodeListDecoder ServiceContext
contextDecoder = KDL.nodeWith "context" $ do
  contextName <- KDL.argWith (ContextName <$> KDL.string)
  subContexts <- KDL.children (KDL.many contextDecoder)
  pure ServiceContext {contextName, subContexts}

contextReferenceDecoder :: NodeListDecoder ContextName
contextReferenceDecoder =
  KDL.nodeWith "in-context" $
    ContextName <$> KDL.argWith KDL.string

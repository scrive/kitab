module Parser.V1.Entity where

import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import KDL

import Core.Model.ContextName
import Core.Model.Entity
import Core.Model.PortNode
import Parser.V1.EntityName
import Parser.V1.PortNode
import Parser.V1.ServiceContext

data EntityChild
  = EntityPort PortNode
  | EntityContext ContextName
  deriving stock (Eq, Ord, Show)

getEntityPort :: EntityChild -> Maybe PortNode
getEntityPort (EntityPort p) = Just p
getEntityPort _ = Nothing

getEntityContext :: EntityChild -> Maybe ContextName
getEntityContext (EntityContext c) = Just c
getEntityContext _ = Nothing

entityDecoder :: NodeListDecoder Entity
entityDecoder = KDL.nodeWith "entity" $ do
  entityName <- KDL.argWith entityNameDecoder
  mixedChildren <-
    KDL.children . KDL.many $
      (EntityPort <$> portDecoder)
        <|> (EntityContext <$> contextReferenceDecoder)

  let ports = Maybe.mapMaybe getEntityPort mixedChildren
  let context = Maybe.listToMaybe $ Maybe.mapMaybe getEntityContext mixedChildren
  let entityInfo = EntityInfo {entityPorts = Set.fromList ports, entityContext = context}
  pure Entity {entityName, entityInfo}

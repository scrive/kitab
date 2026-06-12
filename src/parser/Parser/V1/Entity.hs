module Parser.V1.Entity where

import Data.Set qualified as Set
import GHC.Generics
import KDL

import Core.Model.ContextName
import Core.Model.Entity
import Core.Model.PortNode
import Parser.Util (pickAll, pickOne)
import Parser.V1.EntityName
import Parser.V1.PortNode
import Parser.V1.ServiceContext

data EntityChild
  = EntityPort PortNode
  | EntityContext ContextName
  deriving stock (Eq, Ord, Show, Generic)

entityDecoder :: NodeListDecoder Entity
entityDecoder = KDL.nodeWith "entity" $ do
  entityName <- KDL.argWith entityNameDecoder
  mixedChildren <-
    KDL.children . KDL.many $
      (EntityPort <$> portDecoder)
        <|> (EntityContext <$> contextReferenceDecoder)

  let entityPorts = Set.fromList $ pickAll #_EntityPort mixedChildren
  let entityContext = pickOne #_EntityContext mixedChildren
  let entityInfo = EntityInfo {entityPorts, entityContext}
  pure Entity {entityName, entityInfo}

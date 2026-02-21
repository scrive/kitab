module Core.Model.Entity where

import Data.Set (Set)
import GHC.Generics

import Core.Model.ContextName
import Core.Model.EntityName
import Core.Model.PortNode

data Entity = Entity
  { entityName :: EntityName
  , entityInfo :: EntityInfo
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving
    (Display)
    via (ShowInstance Entity)

data EntityInfo = EntityInfo
  { entityPorts :: Set PortNode
  , entityContext :: Maybe ContextName
  }
  deriving stock (Eq, Show, Ord, Generic)

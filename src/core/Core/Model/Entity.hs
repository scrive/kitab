module Core.Model.Entity where

import Data.Set (Set)
import GHC.Generics

import Core.Model.PortNode
import Core.Model.ServiceName

data Entity = Entity
  { entityName :: ServiceName
  , entityInfo :: EntityInfo
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving
    (Display)
    via (ShowInstance Entity)

data EntityInfo = EntityInfo
  { entityPorts :: Set PortNode
  }
  deriving stock (Eq, Show, Ord, Generic)

module Core.Model.ContextEntity where

import Data.Set (Set)
import GHC.Generics

import Core.Model.PortNode
import Core.Model.Service

data ContextEntity = ContextEntity
  { entityName :: ServiceName
  , entityInfo :: EntityInfo
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving
    (Display)
    via (ShowInstance ContextEntity)

data EntityInfo = EntityInfo
  { entityPorts :: Set PortNode
  }
  deriving stock (Eq, Show, Ord, Generic)

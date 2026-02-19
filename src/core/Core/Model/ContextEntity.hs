module Core.Model.ContextEntity where

import Data.Set (Set)
import Data.String
import GHC.Generics
import Prettyprinter

import Core.Model.PortNode

newtype EntityName = EntityName Text
  deriving newtype (Eq, Ord, Show, IsString, Pretty, Display)

data ContextEntity = ContextEntity
  { entityName :: EntityName
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

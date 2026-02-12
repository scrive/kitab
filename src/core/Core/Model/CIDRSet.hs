module Core.Model.CIDRSet where

import Data.Word
import GHC.Generics

data CIDRSet = CIDRSet
  { items :: List CIDRSetItem
  , ports :: List PortNode
  }
  deriving stock (Eq, Ord, Show)

data CIDRSetItem
  = CIDR Text Text
  | Except Text Text
  deriving stock (Eq, Ord, Show, Generic)

data PortNode = PortNode
  { port :: Word16
  , protocol :: Text
  }
  deriving stock (Eq, Show, Ord)
  deriving
    (Display)
    via (ShowInstance PortNode)

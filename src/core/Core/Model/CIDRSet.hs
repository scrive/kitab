module Core.Model.CIDRSet where

import Data.Word
import GHC.Generics

data CIDRSet = CIDRSet
  { items :: List CIDRSetItem
  , ports :: List Word16
  }
  deriving stock (Eq, Ord, Show)

data CIDRSetItem
  = CIDR Text Text
  | Except Text Text
  deriving stock (Eq, Ord, Show, Generic)

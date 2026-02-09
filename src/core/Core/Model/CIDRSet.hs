module Core.Model.CIDRSet where

import GHC.Generics

newtype CIDRSet = CIDRSet (List CIDRSetItem)
  deriving newtype (Eq, Ord, Show)

data CIDRSetItem
  = CIDR Text Text
  | Except Text Text
  deriving stock (Eq, Ord, Show, Generic)

module Core.Model.CIDRSet where

import GHC.Generics

import Core.Model.PortNode

data CIDRSet = CIDRSet
  { setName :: Text
  , items :: List CIDRSetItem
  , ports :: List PortNode
  }
  deriving stock (Eq, Ord, Show)

data CIDRSetItem
  = CIDR Text Text
  | Except Text Text
  deriving stock (Eq, Ord, Show, Generic)

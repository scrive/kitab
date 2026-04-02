module Core.Model.CIDRSet where

import GHC.Generics

import Core.Model.PortNode

data CIDRSet (var :: Type) = CIDRSet
  { setName :: Text
  , items :: List (CIDRSetItem var)
  , ports :: List PortNode
  }
  deriving stock (Eq, Ord, Show)

data CIDRSetItem (var :: Type)
  = CIDR (Either var (Text, Text))
  | Except (Either var (Text, Text))
  deriving stock (Eq, Ord, Show, Generic)

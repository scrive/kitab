module Core.Model.CIDRSet where

import Data.List.NonEmpty (NonEmpty)

import Core.Model.ContextName
import Core.Model.PortNode

data CIDRSet (var :: Type) = CIDRSet
  { setName :: Text
  , cidrRules :: NonEmpty (CidrRuleNode var)
  , ports :: List PortNode
  , context :: Maybe ContextName
  , rendererProps :: Map Text Text
  }
  deriving stock (Eq, Ord, Show)

-- | A CIDR address with its optional label, or a not-yet-resolved variable
type CidrEntry var = Either var (Tuple2 Text (Maybe Text))

data CidrRuleNode (var :: Type) = CidrRuleNode
  { cidr :: CidrEntry var
  , excepts :: List (CidrEntry var)
  }
  deriving stock (Eq, Ord, Show)

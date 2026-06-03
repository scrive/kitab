module Render.Cilium.Types.CIDRRule
  ( CIDR (..)
  , CIDRRule (..)
  , fromCidrRuleNode
  , prettyCIDR
  , prettyCIDRRule
  ) where

import Data.List qualified as List
import Data.Void
import Prettyprinter

import Core.Model.CIDRSet
import Render.Cilium.Utils

-- | A CIDR prefix, with an optional human-readable label rendered as a YAML comment
data CIDR = CIDR
  { cidrAddress :: Text
  , cidrLabel :: Maybe Text
  }
  deriving stock (Eq, Ord, Show)

-- | Matches Cilium's CIDRRule struct (json tags: cidr, except)
data CIDRRule = CIDRRule
  { cidr :: CIDR
  , except :: List CIDR
  }
  deriving stock (Eq, Ord, Show)

-- | Convert a fully-resolved CidrRuleNode (no variables remain) to a CIDRRule
fromCidrRuleNode :: CidrRuleNode Void -> CIDRRule
fromCidrRuleNode CidrRuleNode {cidr, excepts} =
  CIDRRule
    { cidr = toCIDR cidr
    , except = fmap toCIDR excepts
    }
  where
    toCIDR (Right (addr, label)) = CIDR addr label
    toCIDR (Left v) = absurd v

prettyCIDR :: CIDR -> Doc ann
prettyCIDR CIDR {cidrAddress, cidrLabel} =
  maybe quotedAddress (\lbl -> quotedAddress <+> "#" <+> pretty lbl) cidrLabel
  where
    quotedAddress = dquotes (pretty cidrAddress)

prettyCIDRRule :: CIDRRule -> Doc ann
prettyCIDRRule CIDRRule {cidr, except} =
  vsep $
    [keyValue "cidr" (prettyCIDR cidr)]
      <> if List.null except
        then []
        else [listBlock "except" (listItem . prettyCIDR) except]

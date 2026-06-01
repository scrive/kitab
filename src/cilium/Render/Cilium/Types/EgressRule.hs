module Render.Cilium.Types.EgressRule
  ( EgressRule (..)
  , emptyEgressRule
  , prettyEgressRule
  , EndpointSelector (..)
  , prettyEndpointSelector
  ) where

import Data.Map.Strict qualified as Map
import Prettyprinter

import Core.Model.EntityName
import Render.Cilium.Types
import Render.Cilium.Types.CIDRRule
import Render.Cilium.Utils

-- | Matches Cilium's EgressRule struct — all destination fields are optional
data EgressRule = EgressRule
  { toEndpoints :: Maybe (List EndpointSelector)
  , toCIDRSet :: Maybe (List CIDRRule)
  , toEntities :: Maybe (List EntityName)
  , toFQDNs :: Maybe (List FQDNSelector)
  , toPorts :: Maybe (List PortRule)
  }
  deriving stock (Eq, Ord, Show)

-- | An egress rule with no field set; construct rules with record updates on this
emptyEgressRule :: EgressRule
emptyEgressRule =
  EgressRule
    { toEndpoints = Nothing
    , toCIDRSet = Nothing
    , toEntities = Nothing
    , toFQDNs = Nothing
    , toPorts = Nothing
    }

prettyEgressRule :: EgressRule -> Doc ann
prettyEgressRule EgressRule {toEndpoints, toCIDRSet, toEntities, toFQDNs, toPorts} =
  listItem . vsep $
    catMaybes
      [ toEndpoints <&> listBlock "toEndpoints" (listItem . prettyEndpointSelector)
      , toCIDRSet <&> listBlock "toCIDRSet" (listItem . prettyCIDRRule)
      , toEntities <&> listBlock "toEntities" (listItem . pretty)
      , toFQDNs <&> listBlock "toFQDNs" (listItem . prettyFQDNSelector)
      , toPorts <&> listBlock "toPorts" prettyPortRule
      ]

newtype EndpointSelector = EndpointSelector
  { matchLabels :: Map Text Text
  }
  deriving stock (Show, Eq, Ord)

prettyEndpointSelector :: EndpointSelector -> Doc ann
prettyEndpointSelector (EndpointSelector labels) =
  listBlock "matchLabels" renderLabel (Map.toList labels)
  where
    renderLabel (k, v) = keyValue k (dquotes $ pretty v)

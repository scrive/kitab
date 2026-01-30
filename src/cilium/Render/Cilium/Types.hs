{-# LANGUAGE RecordWildCards #-}

module Render.Cilium.Types where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Prettyprinter

-- | Top-level Cilium Policy
data CiliumNetworkPolicy = CiliumNetworkPolicy
  { apiVersion :: Text
  , kind :: Text
  , metadata :: Metadata
  , spec :: PolicySpec
  }
  deriving (Show, Eq, Ord)

instance Pretty CiliumNetworkPolicy where
  pretty CiliumNetworkPolicy {..} =
    vsep
      [ keyValue "apiVersion" (pretty apiVersion)
      , keyValue "kind" (pretty kind)
      , keyBlock "metadata" (pretty metadata)
      , keyBlock "spec" (pretty spec)
      ]

newtype Metadata = Metadata
  { name :: Text
  }
  deriving newtype (Show, Eq, Ord, Display)

instance Pretty Metadata where
  pretty Metadata {..} = keyValue "name" (pretty name)

-- | The Policy Specification
data PolicySpec = PolicySpec
  { endpointSelector :: EndpointSelector
  , egress :: [EgressRule]
  }
  deriving (Show, Eq, Ord)

instance Pretty PolicySpec where
  pretty PolicySpec {..} =
    vsep
      [ keyBlock "endpointSelector" (pretty endpointSelector)
      , keyBlock "egress" (vsep $ map (("-" <+>) . align . pretty) egress)
      ]

-- | Selectors (used for finding the source Pods or destination Endpoints)
newtype EndpointSelector = EndpointSelector
  { matchLabels :: Map Text Text
  }
  deriving (Show, Eq, Ord)

instance Pretty EndpointSelector where
  pretty (EndpointSelector labels) =
    keyBlock "matchLabels" (vsep $ map renderLabel (Map.toList labels))
    where
      renderLabel (k, v) = keyValue k (dquotes $ pretty v)

-- | A single Egress Rule (mix and match toFQDNs, toEndpoints, and toPorts)
newtype EgressRule = EgressRule
  { egressRuleItems :: List EgressRuleItem
  }
  deriving (Show, Eq, Ord)

instance Pretty EgressRule where
  pretty EgressRule {..} = vsep $ map pretty egressRuleItems

-- | An item in a single Egress Rule
data EgressRuleItem
  = -- | For external/fqdn-based rules
    ToFQDN FQDNMatch
  | -- | For internal/k8s-based rules (e.g. DNS)
    ToEndpoint EndpointSelector
  | -- | For allowed ports/protocols
    ToPort PortRule
  deriving (Show, Eq, Ord)

instance Pretty EgressRuleItem where
  pretty (ToFQDN fqdn) = keyBlock "toFQDNs" $ "-" <+> align (pretty fqdn)
  pretty (ToEndpoint ep) = keyBlock "toEndpoints" $ "-" <+> align (pretty ep)
  pretty (ToPort pr) = keyBlock "toPorts" $ "-" <+> align (pretty pr)

-- | Represents { matchName: "example.com" }
newtype FQDNMatch = FQDNMatch
  { matchName :: Text
  }
  deriving (Show, Eq, Ord)

instance Pretty FQDNMatch where
  pretty (FQDNMatch name) = keyValue "matchName" (dquotes $ pretty name)

-- | Grouping of ports
newtype PortRule = PortRule
  { ports :: [PortProtocol]
  }
  deriving (Show, Eq, Ord)

instance Pretty PortRule where
  pretty (PortRule ports) =
    keyBlock "ports" (vsep $ map (("-" <+>) . align . pretty) ports)

-- | Specific Port/Protocol pair
data PortProtocol = PortProtocol
  { port :: Text -- Text is used because ports can sometimes be named
  , protocol :: Text -- "TCP", "UDP"
  }
  deriving (Show, Eq, Ord)

instance Pretty PortProtocol where
  pretty (PortProtocol p proto) =
    vsep
      [ keyValue "port" (dquotes $ pretty p)
      , keyValue "protocol" (pretty proto)
      ]

-- | Helper for "key: value"
keyValue :: Text -> Doc ann -> Doc ann
keyValue k v = pretty k <> ":" <+> v

-- | Helper for "key:" followed by a nested block
keyBlock :: Text -> Doc ann -> Doc ann
keyBlock k v = pretty k <> ":" <> hardline <> nest 2 v

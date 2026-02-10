{-# LANGUAGE RecordWildCards #-}

module Render.Cilium.Types where

import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Prettyprinter

import Core.Model.CIDRSet

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
  pretty Metadata {..} = keyValue "name" (dquotes $ pretty name)

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
      , keyBlock "egress" (vsep $ List.map (("-" <+>) . align . pretty) egress)
      ]

-- | Selectors (used for finding the source Pods or destination Endpoints)
newtype EndpointSelector = EndpointSelector
  { matchLabels :: Map Text Text
  }
  deriving (Show, Eq, Ord)

instance Pretty EndpointSelector where
  pretty (EndpointSelector labels) =
    keyBlock "matchLabels" (vsep $ List.map renderLabel (Map.toList labels))
    where
      renderLabel (k, v) = keyValue k (dquotes $ pretty v)

-- | A single Egress Rule (mix and match toFQDNs, toEndpoints, and toPorts)
newtype EgressRule = EgressRule
  { egressRuleItems :: List EgressRuleItem
  }
  deriving (Show, Eq, Ord)

instance Pretty EgressRule where
  pretty EgressRule {..} = align . vsep $ List.map pretty egressRuleItems

-- | An item in a single Egress Rule
data EgressRuleItem
  = -- | For external/fqdn-based rules
    ToFQDN FQDNMatch PortRule
  | -- | For internal/k8s-based rules (e.g. DNS)
    ToEndpoint EndpointSelector
  | -- | For allowed ports/protocols
    ToPort PortRule (Maybe DNSMatch)
  | -- |  For allowed IP ranges
    ToCIDRSet CIDRSet
  deriving (Show, Eq, Ord)

instance Pretty EgressRuleItem where
  pretty (ToFQDN fqdn ports) =
    keyBlock "toFQDNs" $
      vsep
        [ "-" <+> align (pretty fqdn)
        , "-" <+> pretty ports
        ]
  pretty (ToEndpoint ep) = keyBlock "toEndpoints" $ "-" <+> align (pretty ep)
  pretty (ToPort portRules dnsMatch) =
    keyBlock "toPorts" . nest 2 $
      vsep
        [ "-" <+> pretty portRules
        , keyBlock "rules" . keyBlock "dns" $ "-" <+> pretty dnsMatch
        ]
  pretty (ToCIDRSet (CIDRSet cidrs ports)) =
    vsep
      [ (keyBlock "toCIDRSets" . indent 2) . vsep $
          List.map
            ( \case
                CIDR cidr name ->
                  keyValue "cidr" (pretty cidr <> " # " <> pretty name)
                Except cidr reason ->
                  keyValue "except" (pretty cidr <> " # " <> pretty reason)
            )
            cidrs
      , if List.null ports
          then mempty
          else
            keyBlock "toPorts" . indent 2 $
              keyBlock
                "ports"
                ( indent 2 $
                    vsep
                      ( List.map
                          ( \p ->
                              vsep
                                [ keyValue "- port" (pretty p)
                                , keyValue "  protocol" "TCP"
                                ]
                          )
                          ports
                      )
                )
      ]

-- | Represents { matchName: "example.com" }
newtype FQDNMatch = FQDNMatch
  { matchName :: Text
  }
  deriving (Show, Eq, Ord)

instance Pretty FQDNMatch where
  pretty (FQDNMatch name) = keyValue "matchName" (dquotes $ pretty name)

newtype DNSMatch = DNSMatch
  { dnsMatchNAme :: Text
  }
  deriving (Show, Eq, Ord)

instance Pretty DNSMatch where
  pretty (DNSMatch name) = keyValue "matchPattern" (dquotes $ pretty name)

-- | Grouping of ports
newtype PortRule = PortRule
  { ports :: [PortProtocol]
  }
  deriving (Show, Eq, Ord)

instance Pretty PortRule where
  pretty (PortRule ports) =
    keyBlock "ports" (vsep $ List.map (("-" <+>) . nest 2 . pretty) ports)

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
keyBlock k v = pretty k <> ":" <> hardline <> indent 2 v

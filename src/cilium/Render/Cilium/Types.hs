{-# LANGUAGE RecordWildCards #-}
{- HLINT ignore "Redundant $" -}

module Render.Cilium.Types where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Void
import Prettyprinter

import Core.Model.CIDRSet
import Core.Model.EntityName
import Core.Model.PortNode

-- | Top-level Cilium Policy
data CiliumNetworkPolicy = CiliumNetworkPolicy
  { apiVersion :: Text
  , kind :: Text
  , metadata :: Metadata
  , spec :: PolicySpec
  }
  deriving stock (Show, Eq, Ord)

instance Pretty CiliumNetworkPolicy where
  pretty CiliumNetworkPolicy {..} =
    vsep
      [ "---"
      , keyValue "apiVersion" (pretty apiVersion)
      , keyValue "kind" (pretty kind)
      , keyBlock "metadata" (indent 2 $ pretty metadata)
      , keyBlock "spec" (indent 2 $ pretty spec)
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
  , egress :: List EgressRule
  }
  deriving stock (Show, Eq, Ord)

instance Pretty PolicySpec where
  pretty PolicySpec {..} =
    vsep
      [ keyBlock "endpointSelector" (indent 2 $ pretty endpointSelector)
      , keyBlock "egress" (indent 2 $ vsep $ List.map (("-" <+>) . align . pretty) egress)
      ]

-- | Selectors (used for finding the source Pods or destination Endpoints)
newtype EndpointSelector = EndpointSelector
  { matchLabels :: Map Text Text
  }
  deriving stock (Show, Eq, Ord)

instance Pretty EndpointSelector where
  pretty (EndpointSelector labels) =
    keyBlock "matchLabels" (indent 2 $ vsep $ List.map renderLabel (Map.toList labels))
    where
      renderLabel (k, v) = keyValue k (dquotes $ pretty v)

-- | A single Egress Rule (mix and match toFQDNs, toEndpoints, and toPorts)
newtype EgressRule = EgressRule
  { egressRuleItems :: List EgressRuleItem
  }
  deriving stock (Show, Eq, Ord)

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
    ToCIDRSet (CIDRSet Void)
  | -- | For Cilium Entities like "host", "world", etc.
    -- See https://docs.cilium.io/en/stable/security/policy/language/#entities-based
    ToEntity EntityName PortRule
  deriving stock (Show, Eq, Ord)

instance Pretty EgressRuleItem where
  pretty = \case
    (ToFQDN fqdn portRule) ->
      keyBlock "toFQDNs" $
        indent 2 $
          vsep
            [ "-" <+> align (pretty fqdn)
            , indent 2 $
                keyBlock
                  "toPorts"
                  (indent 2 $ prettyPortRule portRule)
            ]
    (ToEndpoint ep) -> keyBlock "toEndpoints" $ indent 2 $ "-" <+> align (pretty ep)
    (ToPort portRules dnsMatch) ->
      keyBlock "toPorts" $
        indent 2 $
          vsep
            [ prettyPortRule portRules
            , indent 2 $ keyBlock "rules" $ indent 2 $ keyBlock "dns" $ indent 2 $ "-" <+> pretty dnsMatch
            ]
    (ToCIDRSet (CIDRSet _setNname items ports)) ->
      vsep
        [ keyBlock
            "toCIDRSets"
            ( indent 2 $
                vsep
                  ( List.map
                      ( \case
                          CIDR (Right (cidr, name)) ->  indent 2 $
                            keyValue "cidr" (pretty cidr <> " # " <> pretty name)
                          Except (Right (cidr, reason)) -> indent 2 $
                            keyValue "except" (pretty cidr <> " # " <> pretty reason)
                      )
                      items
                  )
            )
        , if List.null ports
            then mempty
            else
              keyBlock "toPorts" $ indent 2 $
                keyBlock
                  "- ports"
                  ( indent 2 $
                      vsep
                        ( List.map
                            ( \p ->
                                vsep
                                  [ keyValue "- port" (pretty p.port)
                                  , keyValue "  protocol" (pretty p.protocol)
                                  ]
                            )
                            ports
                        )
                  )
        ]
    ToEntity name (PortRule ports) ->
      keyBlock "toEntities" $
        vsep
          [ indent 2 $ "-" <+> pretty name
          , if List.null ports
              then mempty
              else
                keyBlock "toPorts" $ indent 2 $
                  keyBlock
                    "- ports"
                    ( indent 4 $
                        vsep
                          ( List.map
                              ( \p ->
                                  vsep
                                    [ keyValue "- port" (pretty p.port)
                                    , keyValue "  protocol" (pretty p.protocol)
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
  deriving stock (Show, Eq, Ord)

instance Pretty FQDNMatch where
  pretty (FQDNMatch name) = keyValue "matchName" (dquotes $ pretty name)

newtype DNSMatch = DNSMatch
  { dnsMatchNAme :: Text
  }
  deriving stock (Show, Eq, Ord)

instance Pretty DNSMatch where
  pretty (DNSMatch name) = keyValue "matchPattern" (dquotes $ pretty name)

-- | Grouping of ports
newtype PortRule = PortRule
  { ports :: List PortProtocol
  }
  deriving stock (Show, Eq, Ord)

prettyPortRule :: PortRule -> Doc ann
prettyPortRule (PortRule ports) =
  keyBlock "- ports" (indent 4 $ vsep $ List.map prettyPortProtocol ports)

-- | Specific Port/Protocol pair
data PortProtocol = PortProtocol
  { port :: Text -- Text is used because ports can sometimes be named
  , protocol :: Text -- "TCP", "UDP"
  }
  deriving stock (Show, Eq, Ord)

prettyPortProtocol :: PortProtocol -> Doc ann
prettyPortProtocol (PortProtocol p proto) =
  vsep
    [ keyValue "- port" (dquotes $ pretty p)
    , indent 2 $ keyValue "protocol" (pretty proto)
    ]

-- | Helper for "key: value"
keyValue :: Text -> Doc ann -> Doc ann
keyValue k v = pretty k <> ":" <+> v

-- | Helper for "key:" followed by a nested block
keyBlock :: Text -> Doc ann -> Doc ann
keyBlock k v = pretty k <> ":" <> hardline <> v

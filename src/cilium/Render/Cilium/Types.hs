{-# LANGUAGE RecordWildCards #-}
module Render.Cilium.Types where

import Prettyprinter
import Data.Map.Strict (Map)
import Core.Model
import qualified Data.Map.Strict as Map

-- | Top-level Cilium Policy
data CiliumNetworkPolicy = CiliumNetworkPolicy
  { apiVersion :: Text
  , kind :: Text
  , metadata :: Metadata
  , spec :: PolicySpec
  }
  deriving (Show, Eq)

instance Pretty CiliumNetworkPolicy where
  pretty CiliumNetworkPolicy{..} = vsep
    [ keyValue "apiVersion" (pretty apiVersion)
    , keyValue "kind" (pretty kind)
    , keyBlock "metadata" (pretty metadata)
    , keyBlock "spec" (pretty spec)
    ]

data Metadata = Metadata
  { name :: Text
  }
  deriving (Show, Eq)

instance Pretty Metadata where
  pretty Metadata{..} = keyValue "name" (pretty name)

-- | The Policy Specification
data PolicySpec = PolicySpec
  { endpointSelector :: EndpointSelector
  , egress :: [EgressRule]
  }
  deriving (Show, Eq)

instance Pretty PolicySpec where
  pretty PolicySpec{..} = vsep
    [ keyBlock "endpointSelector" (pretty endpointSelector)
    , keyBlock "egress" (vsep $ map (("-" <+>) . align . pretty) egress)
    ]

-- | Selectors (used for finding the source Pods or destination Endpoints)
newtype EndpointSelector = EndpointSelector
  { matchLabels :: Map Text Text
  }
  deriving (Show, Eq)

instance Pretty EndpointSelector where
  pretty (EndpointSelector labels) = 
    keyBlock "matchLabels" (vsep $ map renderLabel (Map.toList labels))
    where
      renderLabel (k, v) = keyValue k (dquotes $ pretty v)

-- | A single Egress Rule (mix and match toFQDNs, toEndpoints, and toPorts)
data EgressRule = EgressRule
  { toFQDNs :: Maybe [FQDNMatch]
  -- ^ For external/fqdn-based rules
  , toEndpoints :: Maybe [EndpointSelector]
  -- ^ For internal/k8s-based rules (e.g. DNS)
  , toPorts :: [PortRule]
  -- ^ List of allowed ports/protocols
  }
  deriving (Show, Eq)

instance Pretty EgressRule where
  pretty EgressRule{..} = vsep $ concat
    [ case toFQDNs of
        Just fqdns -> [keyBlock "toFQDNs" (vsep $ map (("-" <+>) . align . pretty) fqdns)]
        Nothing    -> []
    , case toEndpoints of
        Just eps   -> [keyBlock "toEndpoints" (vsep $ map (("-" <+>) . align . pretty) eps)]
        Nothing    -> []
    , [keyBlock "toPorts" (vsep $ map (("-" <+>) . align . pretty) toPorts)]
    ]

-- | Represents { matchName: "example.com" }
newtype FQDNMatch = FQDNMatch
  { matchName :: Text
  }
  deriving (Show, Eq)

instance Pretty FQDNMatch where
  pretty (FQDNMatch name) = keyValue "matchName" (dquotes $ pretty name)

-- | Grouping of ports
newtype PortRule = PortRule
  { ports :: [PortProtocol]
  }
  deriving (Show, Eq)

instance Pretty PortRule where
  pretty (PortRule ports) = 
    keyBlock "ports" (vsep $ map (("-" <+>) . align . pretty) ports)

-- | Specific Port/Protocol pair
data PortProtocol = PortProtocol
  { port :: Text -- Text is used because ports can sometimes be named
  , protocol :: Text -- "TCP", "UDP"
  }
  deriving (Show, Eq)

instance Pretty PortProtocol where
  pretty (PortProtocol p proto) = vsep
    [ keyValue "port" (dquotes $ pretty p)
    , keyValue "protocol" (pretty proto)
    ]

-- | Convert a Kitab Service to a Cilium Policy
toCiliumPolicy :: Service -> CiliumNetworkPolicy
toCiliumPolicy service =
  CiliumNetworkPolicy
    { apiVersion = "cilium.io/v2"
    , kind = "CiliumNetworkPolicy"
    , metadata = Metadata {name = display service.serviceName <> "-networkpolicy"}
    , spec =
        PolicySpec
          { endpointSelector =
              EndpointSelector $
                singleton "app" (display service.serviceName)
          , egress =
              [ appEgressRule -- The rules from 'depends-on'
              , dnsEgressRule -- The implicit DNS requirement
              ]
          }
    }

    
-- | Helper for "key: value"
keyValue :: Text -> Doc ann -> Doc ann
keyValue k v = pretty k <> ":" <+> v

-- | Helper for "key:" followed by a nested block
keyBlock :: Text -> Doc ann -> Doc ann
keyBlock k v = pretty k <> ":" <> hardline <> nest 2 v


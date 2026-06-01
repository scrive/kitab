{-# LANGUAGE RecordWildCards #-}

module Render.Cilium.Types.NetworkPolicy where

import Prettyprinter

import Render.Cilium.Types.EgressRule
import Render.Cilium.Utils

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
      , keyValue "apiVersion" (dquotes $ pretty apiVersion)
      , keyValue "kind" (pretty kind)
      , keyBlock "metadata" (indent 2 $ pretty metadata)
      , keyBlock "spec" (indent 2 $ pretty spec)
      ]

instance Pretty Metadata where
  pretty Metadata {..} = keyValue "name" (dquotes $ pretty name)

newtype Metadata = Metadata
  { name :: Text
  }
  deriving newtype (Show, Eq, Ord, Display)

instance Pretty PolicySpec where
  pretty PolicySpec {..} =
    vsep
      [ keyBlock "endpointSelector" (indent 2 $ prettyEndpointSelector endpointSelector)
      , listBlock "egress" prettyEgressRule egress
      ]

-- | The Policy Specification
data PolicySpec = PolicySpec
  { endpointSelector :: EndpointSelector
  , egress :: List EgressRule
  }
  deriving stock (Show, Eq, Ord)

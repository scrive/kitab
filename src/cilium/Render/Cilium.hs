module Render.Cilium where

import Data.Map.Strict qualified as Map

import Core.Model
import Render.Cilium.Types

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
                Map.singleton "app" (display service.serviceName)
          , egress =
              [ dnsEgressRule -- The implicit DNS requirement
              ]
          }
    }

dnsEgressRule :: EgressRule
dnsEgressRule =
  EgressRule
    { toFQDNs = Nothing
    , toEndpoints =
        Just
          [ EndpointSelector $
              Map.fromList
                [ ("k8s:io.kubernetes.pod.namespace", "kube-system")
                , ("k8s:k8s-app", "kube-dns")
                ]
          ]
    , toPorts = [PortRule [PortProtocol "53" "UDP", PortProtocol "53" "TCP"]]
    }

module Render.Cilium where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Core.Model
import Render.Cilium.Types

renderCilium :: CiliumNetworkPolicy -> Text
renderCilium = renderStrict . layoutPretty defaultLayoutOptions . pretty

-- | Convert a Kitab Service to a Cilium Policy
toCiliumPolicy :: Map ServiceName ServiceInfo -> Service -> CiliumNetworkPolicy
toCiliumPolicy services service =
  CiliumNetworkPolicy
    { apiVersion = "\"cilium.io/v2\""
    , kind = "CiliumNetworkPolicy"
    , metadata = Metadata {name = display service.serviceName <> "-networkpolicy"}
    , spec =
        PolicySpec
          { endpointSelector =
              EndpointSelector $
                Map.singleton "app" (display service.serviceName)
          , egress =
              dnsEgressRule -- The implicit DNS requirement
                : map (serviceEgressRule services) service.connections
          }
    }

dnsEgressRule :: EgressRule
dnsEgressRule =
  EgressRule
    [ ToEndpoint
        EndpointSelector
          { matchLabels =
              Map.fromList
                [ ("k8s:io.kubernetes.pod.namespace", "kube-system")
                , ("k8s:k8s-app", "kube-dns")
                ]
          }
    , ToPort
        PortRule
          { ports = [PortProtocol "53" "UDP", PortProtocol "53" "TCP"]
          }
    ]

serviceEgressRule :: Map ServiceName ServiceInfo -> Connection -> EgressRule
serviceEgressRule services Connection {connectionWith}
  | Just ServiceInfo {serviceFqdn} <- Map.lookup connectionWith services =
      EgressRule $ maybe [] (pure . ToFQDN . FQDNMatch) serviceFqdn
  | otherwise = error $ "missing service " ++ show connectionWith

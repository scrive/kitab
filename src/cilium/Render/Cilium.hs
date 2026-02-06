module Render.Cilium where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Core.Model.Service
import Core.Model.ServiceContext
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
                : map (serviceEgressRule service.serviceInfo.serviceContext services) service.connections
          }
    }

dnsEgressRule :: EgressRule
dnsEgressRule =
  EgressRule
    [ ToEndpoint
        EndpointSelector
          { matchLabels =
              Map.fromList
                [ ("io.kubernetes.pod.namespace", "kube-system")
                , ("k8s-app", "kube-dns")
                ]
          }
    , ToPort
        PortRule
          { ports = [PortProtocol "53" "UDP"]
          }
        (Just DNSMatch {dnsMatchNAme = "*"})
    ]

serviceEgressRule :: Maybe ServiceContext -> Map ServiceName ServiceInfo -> Connection -> EgressRule
serviceEgressRule mContext services Connection {connectionWith}
  | Just ServiceInfo {serviceContext} <- Map.lookup connectionWith services
  , isJust serviceContext
  , serviceContext == mContext =
      EgressRule . pure $ ToEndpoint (EndpointSelector $ Map.singleton "app" (display connectionWith))
  | Just ServiceInfo {serviceFqdn} <- Map.lookup connectionWith services =
      EgressRule $ maybe [] (\hostname -> pure $ ToFQDN (FQDNMatch hostname) (PortRule [PortProtocol "443" "TCP"])) serviceFqdn
  | otherwise = error $ "missing service " ++ show connectionWith

module Render.Cilium where

import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Core.Model.CIDRSet
import Core.Model.Service
import Core.Model.ServiceContext
import Render.Cilium.Types

renderCilium :: CiliumNetworkPolicy -> Text
renderCilium = renderStrict . layoutPretty defaultLayoutOptions . pretty

-- | Convert a Kitab Service to a Cilium Policy
toCiliumPolicy
  :: Map ServiceName ServiceInfo
  -> Service
  -> CiliumNetworkPolicy
toCiliumPolicy services service =
  CiliumNetworkPolicy
    { apiVersion = "\"cilium.io/v2\""
    , kind = "CiliumNetworkPolicy"
    , metadata = Metadata {name = display service.serviceName <> "-network-policy"}
    , spec =
        PolicySpec
          { endpointSelector =
              EndpointSelector $
                Map.singleton "app" (display service.serviceName)
          , egress =
              [dnsEgressRule] -- The implicit DNS requirement
                <> List.map (serviceEgressRule service.serviceInfo.serviceContext services) service.connections
                <> List.map cidrEgressRule service.cidrSets
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
serviceEgressRule mContext services Connection {connectionWith, connectionPort}
  | Just ServiceInfo {serviceContext} <- Map.lookup connectionWith services
  , isJust serviceContext
  , serviceContext == mContext =
      EgressRule . pure $ ToEndpoint (EndpointSelector $ Map.singleton "app" (display connectionWith))
  | Just ServiceInfo {serviceFqdn} <- Map.lookup connectionWith services
  , let port = maybe "443" display connectionPort =
      EgressRule $ maybe [] (\hostname -> pure $ ToFQDN (FQDNMatch hostname) (PortRule [PortProtocol port "TCP"])) serviceFqdn
  | otherwise = error $ "missing service " ++ show connectionWith

cidrEgressRule :: CIDRSet -> EgressRule
cidrEgressRule = EgressRule . List.singleton . ToCIDRSet

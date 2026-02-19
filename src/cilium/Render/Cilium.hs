module Render.Cilium where

import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Core.Model.CIDRSet
import Core.Model.ContextName
import Core.Model.PortNode
import Core.Model.Service
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

serviceEgressRule :: Maybe ContextName -> Map ServiceName ServiceInfo -> Connection -> EgressRule
serviceEgressRule mContext services Connection {connectionWith, connectionPorts}
  | Just ServiceInfo {serviceContext} <- Map.lookup connectionWith services
  , isJust serviceContext
  , serviceContext == mContext =
      EgressRule . pure $ ToEndpoint (EndpointSelector $ Map.singleton "app" (display connectionWith))
  | Just ServiceInfo {serviceFqdn} <- Map.lookup connectionWith services
  , let ports = Set.toList $ pickPorts services connectionWith connectionPorts =
      EgressRule $
        maybe
          []
          ( \hostname ->
              pure $
                ToFQDN
                  (FQDNMatch hostname)
                  (PortRule (List.map (\portNode -> PortProtocol (display portNode.port) portNode.protocol) ports))
          )
          serviceFqdn
  | otherwise = error $ "missing service " ++ show connectionWith

cidrEgressRule :: CIDRSet -> EgressRule
cidrEgressRule = EgressRule . List.singleton . ToCIDRSet

-- | Select which ports to use for a connection:
-- 1. If no ports are specified by the caller, we use the ones opened by the callee
-- 2. If the caller specifies ports, we check if they are also defined by the callee
-- 3. If not, we fallback to port 443 on TCP
pickPorts :: Map ServiceName ServiceInfo -> ServiceName -> Set PortNode -> Set PortProtocol
pickPorts services with ports
  | Set.null ports
  , Just ServiceInfo {servicePorts} <- Map.lookup with services =
      Set.map (\portNode -> PortProtocol (display portNode.port) portNode.protocol) servicePorts
  | not . Set.null $ ports
  , Just ServiceInfo {servicePorts} <- Map.lookup with services
  , ports `Set.isSubsetOf` servicePorts =
      Set.map (\portNode -> PortProtocol (display portNode.port) portNode.protocol) ports
  | otherwise = Set.singleton (PortProtocol "443" "TCP")

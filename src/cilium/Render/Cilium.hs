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
import Core.Model.Entity
import Core.Model.EntityName
import Core.Model.PortNode
import Core.Model.Service
import Core.Model.ServiceName
import Render.Cilium.Types

renderCilium :: CiliumNetworkPolicy -> Text
renderCilium = renderStrict . layoutPretty defaultLayoutOptions . pretty

-- | Convert a Kitab Service to a Cilium Policy
toCiliumPolicy
  :: Map ServiceName ServiceInfo
  -> Map EntityName EntityInfo
  -> Service
  -> CiliumNetworkPolicy
toCiliumPolicy serviceIndex entityIndex service =
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
                <> List.map (serviceEgressRule service.serviceInfo.serviceContext serviceIndex) service.serviceConnections
                <> List.map cidrEgressRule service.cidrSets
                <> List.map (entityEgressRule entityIndex) service.entityAccesses
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
  , let ports = Set.toList $ pickServicePorts services connectionWith connectionPorts =
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

-- | Select which ports to use for a connection:
-- 1. If no ports are specified by the caller, we use the ones opened by the callee
-- 2. If the caller specifies ports, we check if they are also defined by the callee
-- 3. If not, we fallback to port 443 on TCP
pickServicePorts :: Map ServiceName ServiceInfo -> ServiceName -> Set PortNode -> Set PortProtocol
pickServicePorts services with ports
  | Set.null ports
  , Just ServiceInfo {servicePorts} <- Map.lookup with services =
      Set.map (\portNode -> PortProtocol (display portNode.port) portNode.protocol) servicePorts
  | not . Set.null $ ports
  , Just ServiceInfo {servicePorts} <- Map.lookup with services
  , ports `Set.isSubsetOf` servicePorts =
      Set.map (\portNode -> PortProtocol (display portNode.port) portNode.protocol) ports
  | otherwise = Set.singleton (PortProtocol "443" "TCP")

-- | Select which ports to use for a connection:
-- 1. If no ports are specified by the caller, we use the ones opened by the callee
-- 2. If the caller specifies ports, we check if they are also defined by the callee
-- 3. If not, we fallback to port 443 on TCP
pickEntityPorts :: Map EntityName EntityInfo -> EntityName -> Set PortNode -> Set PortProtocol
pickEntityPorts entities with ports
  | Set.null ports
  , Just EntityInfo {entityPorts} <- Map.lookup with entities =
      Set.map (\portNode -> PortProtocol (display portNode.port) portNode.protocol) entityPorts
  | not . Set.null $ ports
  , Just EntityInfo {entityPorts} <- Map.lookup with entities
  , ports `Set.isSubsetOf` entityPorts =
      Set.map (\portNode -> PortProtocol (display portNode.port) portNode.protocol) ports
  | otherwise = Set.empty

entityEgressRule :: Map EntityName EntityInfo -> EntityAccess -> EgressRule
entityEgressRule entitiesIndex access =
  let ports = pickEntityPorts entitiesIndex access.accessTarget access.accessPorts
  in EgressRule . List.singleton $ ToEntity access.accessTarget (PortRule $ Set.toList ports)

cidrEgressRule :: CIDRSet -> EgressRule
cidrEgressRule = EgressRule . List.singleton . ToCIDRSet

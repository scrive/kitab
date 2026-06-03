module Render.Cilium where

import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void
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
import Render.Cilium.Types.CIDRRule
import Render.Cilium.Types.EgressRule
import Render.Cilium.Types.NetworkPolicy

renderCilium :: CiliumNetworkPolicy -> Text
renderCilium policy =
  policy
    & pretty
    & layoutPretty defaultLayoutOptions
    & renderStrict
    & (\t -> t <> "\n")

-- | Convert a Kitab Service to a Cilium Policy
toCiliumPolicy
  :: Map ServiceName (ServiceInfo Void)
  -> Map EntityName EntityInfo
  -> Map Text (CIDRSet Void)
  -> Service Void
  -> CiliumNetworkPolicy
toCiliumPolicy serviceIndex entityIndex cidrIndex service =
  CiliumNetworkPolicy
    { apiVersion = "cilium.io/v2"
    , kind = "CiliumNetworkPolicy"
    , metadata = Metadata {name = display service.serviceName <> "-network-policy"}
    , spec =
        PolicySpec
          { endpointSelector =
              EndpointSelector $
                Map.singleton "app.kubernetes.io/name" (display service.serviceName)
          , egress =
              Maybe.mapMaybe (cidrEgressRule cidrIndex) service.cidrConnections
                <> [dnsEgressRule] -- The implicit DNS requirement
                <> List.map (serviceEgressRule service.serviceInfo.serviceContext serviceIndex) service.serviceConnections
                <> List.map (entityEgressRule entityIndex) service.entityAccesses
          }
    }

-- | A toPorts entry for the given ports, or nothing if there are none.
-- Cilium treats a rule without toPorts as "all ports allowed".
portsRule :: List PortProtocol -> Maybe (List PortRule)
portsRule ports
  | List.null ports = Nothing
  | otherwise = Just [PortRule {ports, rules = Nothing}]

dnsEgressRule :: EgressRule
dnsEgressRule =
  emptyEgressRule
    { toEndpoints =
        Just
          [ EndpointSelector
              { matchLabels =
                  Map.fromList
                    [ ("io.kubernetes.pod.namespace", "kube-system")
                    , ("k8s-app", "kube-dns")
                    ]
              }
          ]
    , toPorts =
        Just
          [ PortRule
              { ports = [PortProtocol "53" "UDP"]
              , rules = Just $ L7Rules [MatchPattern "*"]
              }
          ]
    }

-- | How a service can reach a connection target in a Cilium policy, if at all.
-- This is the single source of truth for the reachability rule: both
-- 'serviceEgressRule' and 'Driver.Cilium.validateConnections' consume it.
data Reachability
  = SameContext
  | ViaFqdn Text
  | Unreachable

reachability :: Maybe ContextName -> ServiceInfo Void -> Reachability
reachability mContext ServiceInfo {serviceContext, serviceFqdn}
  | isJust serviceContext
  , serviceContext == mContext =
      SameContext
  | Just (Right hostname) <- serviceFqdn = ViaFqdn hostname
  | otherwise = Unreachable

serviceEgressRule
  :: Maybe ContextName
  -> Map ServiceName (ServiceInfo Void)
  -> Connection
  -> EgressRule
serviceEgressRule mContext services Connection {connectionWith, connectionPorts} =
  case Map.lookup connectionWith services of
    -- Unreachable: Driver.Cilium.validateConnections rejects these configurations before rendering
    Nothing -> error $ "missing service " ++ show connectionWith
    Just serviceInfo ->
      case reachability mContext serviceInfo of
        SameContext ->
          emptyEgressRule
            { toEndpoints = Just [EndpointSelector $ Map.singleton "app" (display connectionWith)]
            }
        ViaFqdn hostname ->
          emptyEgressRule
            { toFQDNs = Just [MatchName hostname]
            , toPorts = portsRule . Set.toList $ pickServicePorts serviceInfo connectionPorts
            }
        -- Refuse to emit a rule with no destination: in Cilium, an egress rule with
        -- only toPorts allows the ports towards *all* destinations.
        -- Unreachable: Driver.Cilium.validateConnections rejects these configurations before rendering
        Unreachable ->
          error $ "service " ++ show connectionWith ++ " has no fqdn and is not in the same context"

-- | Select which ports to use for a connection:
-- 1. If no ports are specified by the caller, use the ones opened by the callee
-- 2. If the caller specifies ports and they are a subset of the callee's ports, use them
-- 3. Otherwise fall back to port 443/TCP
pickServicePorts :: ServiceInfo Void -> Set PortNode -> Set PortProtocol
pickServicePorts ServiceInfo {servicePorts} ports
  | Set.null ports = Set.map fromPortNode servicePorts
  | ports `Set.isSubsetOf` servicePorts = Set.map fromPortNode ports
  | otherwise = Set.singleton (PortProtocol "443" "TCP")

-- | Select which ports to use for an entity connection: the caller's ports if
-- they are a subset of the entity's, otherwise the entity's own ports
pickEntityPorts :: Map EntityName EntityInfo -> EntityName -> Set PortNode -> Set PortProtocol
pickEntityPorts entities with ports =
  case Map.lookup with entities of
    Nothing -> Set.empty
    Just EntityInfo {entityPorts}
      | Set.null ports -> Set.map fromPortNode entityPorts
      | ports `Set.isSubsetOf` entityPorts -> Set.map fromPortNode ports
      | otherwise -> Set.empty

entityEgressRule :: Map EntityName EntityInfo -> EntityAccess -> EgressRule
entityEgressRule entitiesIndex access =
  emptyEgressRule
    { toEntities = Just [access.accessTarget]
    , toPorts = portsRule . Set.toList $ pickEntityPorts entitiesIndex access.accessTarget access.accessPorts
    }

cidrEgressRule :: Map Text (CIDRSet Void) -> CIDRConnection -> Maybe EgressRule
cidrEgressRule cidrIndex connection =
  Map.lookup connection.connectTarget cidrIndex <&> \cidrSet ->
    emptyEgressRule
      { toCIDRSet = Just . NE.toList $ fmap fromCidrRuleNode cidrSet.cidrRules
      , toPorts = portsRule $ fmap fromPortNode cidrSet.ports
      }

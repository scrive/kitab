module Render.Cilium where

import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Core.Model.CIDRSet
import Core.Model.PortNode
import Render.Cilium.Resolved
import Render.Cilium.Types
import Render.Cilium.Types.CIDRRule
import Render.Cilium.Types.EgressRule
import Render.Cilium.Types.NetworkPolicy

renderCilium :: Bool -> CiliumNetworkPolicy -> Text
renderCilium enableVersionStamp policy =
  policy
    & prettyNetworkPolicy enableVersionStamp
    & layoutPretty defaultLayoutOptions
    & renderStrict
    & (\t -> t <> "\n")

-- | Convert a resolved Kitab Service to a Cilium Policy
toCiliumPolicy :: ResolvedService -> CiliumNetworkPolicy
toCiliumPolicy service =
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
              List.map cidrEgressRule service.cidrTargets
                <> [dnsEgressRule] -- The implicit DNS requirement
                <> List.map serviceEgressRule service.serviceTargets
                <> List.map entityEgressRule service.entityTargets
          }
    }

-- | A toPorts entry for the given ports, or nothing if there are none.
-- Cilium treats a rule without toPorts as "all ports allowed".
portsRule :: List PortProtocol -> Maybe (List PortRule)
portsRule ports
  | List.null ports = Nothing
  | otherwise = Just [PortRule {ports, rules = Nothing}]

-- | Like 'portsRule', for the resolved 'PortNode' sets
portNodesRule :: Set PortNode -> Maybe (List PortRule)
portNodesRule = portsRule . Set.toList . Set.map fromPortNode

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

serviceEgressRule :: ResolvedConnection -> EgressRule
serviceEgressRule connection =
  case connection.connectionRoute of
    SameContext ->
      emptyEgressRule
        { toEndpoints = Just [EndpointSelector $ Map.singleton "app" (display connection.connectionTarget)]
        }
    ViaFqdn hostname ->
      emptyEgressRule
        { toFQDNs = Just [MatchName hostname]
        , toPorts = portNodesRule connection.connectionPorts
        }

entityEgressRule :: ResolvedEntityAccess -> EgressRule
entityEgressRule access =
  emptyEgressRule
    { toEntities = Just [access.accessTarget]
    , toPorts = portNodesRule access.accessPorts
    }

cidrEgressRule :: CIDRSet Void -> EgressRule
cidrEgressRule cidrSet =
  emptyEgressRule
    { toCIDRSet = Just . NE.toList $ fmap fromCidrRuleNode cidrSet.cidrRules
    , toPorts = portsRule $ fmap fromPortNode cidrSet.ports
    }

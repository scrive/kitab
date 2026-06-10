module Test.Render.CiliumTests where

import Data.ByteString.Lazy (LazyByteString)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Test.Tasty
import Test.Tasty.Golden
import Validation

import Core.Graph
import Core.Model.Entity
import Core.Model.Inventory.Aggregated
import Core.Model.InventoryVariable
import Core.Model.PortNode
import Core.Model.Service
import Core.Validation
import Driver.Variable
import Parser.V1.Types
import Render.Cilium qualified as Cilium
import Render.Cilium.Resolved
import Test.Utils

test :: TestTree
test =
  testGroup
    "Cilium rendering golden test"
    [ goldenVsStringDiff
        "Services"
        diffCmd
        "test/golden/network-policy.yaml"
        renderService
    , goldenVsStringDiff
        "ToCIDRset"
        diffCmd
        "test/golden/cidr-network-policy.yaml"
        renderCIDRSetPolicy
    , testThat
        "Resolution rejects undeclared entity targets"
        rejectUndeclaredEntityTarget
    , testGroup
        "Port selection"
        [ testThat
            "Connection without ports uses the target's ports"
            connectionWithoutPortsUsesTargetPorts
        , testThat
            "Connection ports within the target's ports are kept"
            connectionPortsSubsetIsKept
        , testThat
            "Connection ports equal to the target's ports are kept"
            connectionPortsEqualSetIsKept
        , testThat
            "Connection ports outside the target's ports fall back to 443/TCP"
            connectionPortsOutsideTargetFallBack
        , testThat
            "Connection ports partially overlapping the target's ports fall back to 443/TCP"
            connectionPortsPartialOverlapFallsBack
        , testThat
            "Connection ports with a mismatched protocol fall back to 443/TCP"
            connectionPortsProtocolMismatchFallsBack
        , testThat
            "Entity access without ports uses the entity's ports"
            entityAccessWithoutPortsUsesEntityPorts
        , testThat
            "Entity access ports within the entity's ports are kept"
            entityAccessPortsSubsetIsKept
        , testThat
            "Entity access ports outside the entity's ports fall back to no ports"
            entityAccessPortsOutsideEntityFallBack
        ]
    ]

renderService :: IO LazyByteString
renderService = runTestEff $ do
  let aggregatedInventory = AggregatedInventory mempty mempty
  declarations <- partitionDeclarations <$> assertParseDocument "test/fixtures/multiple-service-definitions.kdl"
  serviceDefinitions <- traverse (resolveServiceVars aggregatedInventory) declarations.services
  let entities = declarations.entities
  cidrDefinitions <- traverse (resolveCIDRVars aggregatedInventory) declarations.cidrs
  let graph = buildGraph serviceDefinitions entities
  let serviceIndex = buildServiceIndex serviceDefinitions
  let entityIndex = buildEntityIndex entities
  let cidrIndex = buildCidrIndex cidrDefinitions
  void . assertRight "Graph is invalid" $ validationToEither (checkGraph graph)
  mediaProxyService <- assertJust "" $ List.find (\s -> s.serviceName == "media-proxy") serviceDefinitions
  resolvedService <-
    assertRight "Service does not resolve" . validationToEither $
      resolveService serviceIndex entityIndex cidrIndex mediaProxyService
  ((pure . TL.encodeUtf8) . T.fromStrict) . Cilium.renderCilium $ Cilium.toCiliumPolicy resolvedService

renderCIDRSetPolicy :: IO LazyByteString
renderCIDRSetPolicy = runTestEff $ do
  let mysqlFqdn =
        InventoryVariable
          { name = "mysql-cluster-cidr"
          , value = "10.147.128.0/24"
          , description = Just "MySQL"
          }
  let aggregatedInventory =
        AggregatedInventory
          { aggregatedAttributes = Map.fromList [("cloud", "aws"), ("env", "dev")]
          , aggregatedVars = Map.fromList [("mysql-cluster-cidr", mysqlFqdn)]
          }
  declarations <- partitionDeclarations <$> assertParseDocument "test/fixtures/cidrset.kdl"
  serviceDefinitions <- traverse (resolveServiceVars aggregatedInventory) declarations.services
  let entities = declarations.entities
  cidrDefinitions <- traverse (resolveCIDRVars aggregatedInventory) declarations.cidrs
  let graph = buildGraph serviceDefinitions entities
  let serviceIndex = buildServiceIndex serviceDefinitions
  let entityIndex = buildEntityIndex entities
  let cidrIndex = buildCidrIndex cidrDefinitions
  void . assertRight "Graph is invalid" $ validationToEither (checkGraph graph)
  myAppService <- assertJust "" $ List.find (\s -> s.serviceName == "my-app") serviceDefinitions
  resolvedService <-
    assertRight "Service does not resolve" . validationToEither $
      resolveService serviceIndex entityIndex cidrIndex myAppService
  ((pure . TL.encodeUtf8) . T.fromStrict) . Cilium.renderCilium $ Cilium.toCiliumPolicy resolvedService

-- | Resolve a single connection from a source service to a target opening
-- @targetPorts@, both in the same context, and return the picked ports.
resolveConnectionPorts :: Set PortNode -> Set PortNode -> TestEff (Set PortNode)
resolveConnectionPorts targetPorts callerPorts = do
  let targetInfo =
        ServiceInfo
          { serviceFqdn = Nothing
          , serviceContext = Just "ctx"
          , servicePorts = targetPorts
          }
  let serviceIndex = Map.singleton "target" targetInfo
  let service =
        emptyService
          { serviceName = "web"
          , serviceInfo = targetInfo {servicePorts = Set.empty}
          , serviceConnections =
              [ Connection
                  { connectionWith = "target"
                  , connectionType = HTTPS
                  , connectionPorts = callerPorts
                  }
              ]
          }
  resolved <-
    assertRight "Service does not resolve" . validationToEither $
      resolveService serviceIndex mempty mempty service
  case resolved.serviceTargets of
    [connection] -> pure connection.connectionPorts
    targets -> assertFailure $ "Expected exactly one resolved connection, got: " <> show targets

-- | Resolve a single entity access to an entity opening @entityPorts@ and
-- return the picked ports.
resolveEntityPorts :: Set PortNode -> Set PortNode -> TestEff (Set PortNode)
resolveEntityPorts entityPorts callerPorts = do
  let entityIndex =
        Map.singleton "kafka" EntityInfo {entityPorts, entityContext = Nothing}
  let service =
        emptyService
          { serviceName = "web"
          , entityAccesses = [EntityAccess {accessTarget = "kafka", accessPorts = callerPorts}]
          }
  resolved <-
    assertRight "Service does not resolve" . validationToEither $
      resolveService mempty entityIndex mempty service
  case resolved.entityTargets of
    [access] -> pure access.accessPorts
    targets -> assertFailure $ "Expected exactly one resolved entity access, got: " <> show targets

connectionWithoutPortsUsesTargetPorts :: TestEff Unit
connectionWithoutPortsUsesTargetPorts = do
  let targetPorts = Set.fromList [PortNode 80 "TCP", PortNode 8080 "TCP"]
  picked <- resolveConnectionPorts targetPorts Set.empty
  assertEqual "Picked ports" targetPorts picked

connectionPortsSubsetIsKept :: TestEff Unit
connectionPortsSubsetIsKept = do
  let targetPorts = Set.fromList [PortNode 80 "TCP", PortNode 8080 "TCP", PortNode 5432 "TCP"]
  picked <- resolveConnectionPorts targetPorts (Set.singleton (PortNode 5432 "TCP"))
  assertEqual "Picked ports" (Set.singleton (PortNode 5432 "TCP")) picked

connectionPortsEqualSetIsKept :: TestEff Unit
connectionPortsEqualSetIsKept = do
  let targetPorts = Set.fromList [PortNode 80 "TCP", PortNode 8080 "TCP"]
  picked <- resolveConnectionPorts targetPorts targetPorts
  assertEqual "Picked ports" targetPorts picked

connectionPortsOutsideTargetFallBack :: TestEff Unit
connectionPortsOutsideTargetFallBack = do
  let targetPorts = Set.singleton (PortNode 80 "TCP")
  picked <- resolveConnectionPorts targetPorts (Set.singleton (PortNode 9999 "TCP"))
  assertEqual "Picked ports" (Set.singleton (PortNode 443 "TCP")) picked

connectionPortsPartialOverlapFallsBack :: TestEff Unit
connectionPortsPartialOverlapFallsBack = do
  let targetPorts = Set.singleton (PortNode 80 "TCP")
  picked <- resolveConnectionPorts targetPorts (Set.fromList [PortNode 80 "TCP", PortNode 9999 "TCP"])
  assertEqual "Picked ports" (Set.singleton (PortNode 443 "TCP")) picked

connectionPortsProtocolMismatchFallsBack :: TestEff Unit
connectionPortsProtocolMismatchFallsBack = do
  let targetPorts = Set.singleton (PortNode 53 "TCP")
  picked <- resolveConnectionPorts targetPorts (Set.singleton (PortNode 53 "UDP"))
  assertEqual "Picked ports" (Set.singleton (PortNode 443 "TCP")) picked

entityAccessWithoutPortsUsesEntityPorts :: TestEff Unit
entityAccessWithoutPortsUsesEntityPorts = do
  let entityPorts = Set.fromList [PortNode 9092 "TCP", PortNode 9093 "TCP"]
  picked <- resolveEntityPorts entityPorts Set.empty
  assertEqual "Picked ports" entityPorts picked

entityAccessPortsSubsetIsKept :: TestEff Unit
entityAccessPortsSubsetIsKept = do
  let entityPorts = Set.fromList [PortNode 9092 "TCP", PortNode 9093 "TCP"]
  picked <- resolveEntityPorts entityPorts (Set.singleton (PortNode 9092 "TCP"))
  assertEqual "Picked ports" (Set.singleton (PortNode 9092 "TCP")) picked

entityAccessPortsOutsideEntityFallBack :: TestEff Unit
entityAccessPortsOutsideEntityFallBack = do
  let entityPorts = Set.singleton (PortNode 9092 "TCP")
  picked <- resolveEntityPorts entityPorts (Set.singleton (PortNode 9999 "TCP"))
  assertEqual "Picked ports" Set.empty picked

rejectUndeclaredEntityTarget :: TestEff Unit
rejectUndeclaredEntityTarget = do
  let service =
        emptyService
          { serviceName = "web"
          , entityAccesses = [EntityAccess {accessTarget = "kafka", accessPorts = mempty}]
          }
  case resolveService mempty mempty mempty service of
    Failure violations ->
      assertEqual "Unexpected resolution errors" [MissingEntity "web" "kafka"] (toList violations)
    Success resolved ->
      assertFailure $ "Resolution accepted an undeclared entity target: " <> show resolved

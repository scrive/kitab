module Test.Render.CiliumTests where

import Data.ByteString.Lazy (LazyByteString)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Test.Tasty
import Test.Tasty.Golden
import Validation

import Core.Graph
import Core.Model.Inventory.Aggregated
import Core.Model.InventoryVariable
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
    ]

renderService :: IO LazyByteString
renderService = runTestEff $ do
  let aggregatedInventory = AggregatedInventory mempty mempty
  declarations <- assertParseDocument "test/fixtures/multiple-service-definitions.kdl"
  serviceDefinitions <-
    mapMaybe
      ( \case
          ServiceDeclaration s -> Just s
          _ -> Nothing
      )
      declarations
      & traverse (resolveServiceVars aggregatedInventory)
  let entities =
        mapMaybe
          ( \case
              EntityDeclaration s -> Just s
              _ -> Nothing
          )
          declarations
  cidrDefinitions <-
    mapMaybe
      ( \case
          CIDRSetDeclaration c -> Just c
          _ -> Nothing
      )
      declarations
      & traverse (resolveCIDRVars aggregatedInventory)
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
  declarations <- assertParseDocument "test/fixtures/cidrset.kdl"
  serviceDefinitions <-
    mapMaybe
      ( \case
          ServiceDeclaration s -> Just s
          _ -> Nothing
      )
      declarations
      & traverse (resolveServiceVars aggregatedInventory)
  let entities =
        mapMaybe
          ( \case
              EntityDeclaration s -> Just s
              _ -> Nothing
          )
          declarations
  cidrDefinitions <-
    mapMaybe
      ( \case
          CIDRSetDeclaration c -> Just c
          _ -> Nothing
      )
      declarations
      & traverse (resolveCIDRVars aggregatedInventory)
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

module Test.Render.PumlTests where

import Algebra.Graph.Labelled (Graph)
import Algebra.Graph.Labelled qualified as Graph
import Algebra.Graph.Labelled.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Optics.Core
import Data.ByteString.Lazy (LazyByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Data.Void (Void)
import Effectful.Error.Static qualified as Error
import Test.Tasty
import Test.Tasty.Golden
import Validation

import CLI.Error (CLIError)
import Core.Graph
import Core.Model.Inventory.Aggregated
import Core.Model.Reference
import Core.Model.CIDRSet
import Core.Model.Service
import Core.Model.ServiceName
import Core.Validation
import Driver.Puml (validateContainers)
import Driver.Variable
import Parser.V1.Types
import Render.Puml qualified as Puml
import Render.Puml.C4Container.Types
import Test.Utils

test :: TestTree
test =
  testGroup
    "C4 rendering golden tests"
    [ goldenVsStringDiff
        "Services"
        diffCmd
        "test/golden/service.puml"
        renderServices
    , goldenVsStringDiff
        "Browser to backend connection"
        diffCmd
        "test/golden/web-app-to-backend.puml"
        renderWebAppToBackend
    , goldenVsStringDiff
        "Service with only a connect node"
        diffCmd
        "test/golden/connect-only.puml"
        renderConnectOnly
    , goldenVsStringDiff
        "Service puml:type renders Container, ContainerDb and ContainerQueue"
        diffCmd
        "test/golden/puml-type.puml"
        renderPumlType
    , testThat
        "Service with an unknown puml:type is rejected by the renderer"
        testUnknownPumlType
    , testThat
        "Isolated service with an unknown puml:type is rejected"
        testIsolatedUnknownPumlType
    , testThat
        "Service with an unrecognized puml: prop key is rejected"
        testUnknownPumlProp
    ]

testIsolatedUnknownPumlType :: TestEff Unit
testIsolatedUnknownPumlType = do
  let serviceInfo = emptyServiceInfo & #rendererProps .~ Map.fromList [("puml:type", "invalid")]
  let serviceIndex = Map.fromList [(ServiceName "postgres", serviceInfo)] :: Map ServiceName (ServiceInfo Void)
  let cidrIndex = Map.empty
  result <- Error.runErrorNoCallStack @(NonEmpty CLIError) $ validateContainers serviceIndex cidrIndex []
  void $ assertLeft "Expected the isolated service to be rejected" result

testUnknownPumlType :: TestEff Unit
testUnknownPumlType = do
  let serviceInfo = emptyServiceInfo & #rendererProps .~ Map.fromList [("puml:type", "invalid")]
  let serviceIndex = Map.fromList [(ServiceName "postgres", serviceInfo)]
  let cidrIndex = Map.empty
  result <- assertLeft "Expected an unknown puml type error" $ toC4Container serviceIndex cidrIndex (ServiceRef (ServiceName "postgres"))
  assertEqual
    "Expected the offending puml type to be reported"
    (InvalidPumlProp InvalidPumlPropError {serviceName = ServiceName "postgres", propKey = "puml:type", providedValue = "invalid", supportedValues = ["database", "queue", "service"]})
    result

testUnknownPumlProp :: TestEff Unit
testUnknownPumlProp = do
  let serviceInfo = emptyServiceInfo & #rendererProps .~ Map.fromList [("puml:tpye", "database")]
  let serviceIndex = Map.fromList [(ServiceName "postgres", serviceInfo)]
  let cidrIndex = Map.empty
  result <- assertLeft "Expected an unknown puml prop error" $ toC4Container serviceIndex cidrIndex (ServiceRef (ServiceName "postgres"))
  assertEqual
    "Expected the offending puml prop key to be reported"
    (UnknownPumlProp UnknownPumlPropError {serviceName = ServiceName "postgres", propKey = "puml:tpye"})
    result

toAdjacencyMap
  :: Map ServiceName (ServiceInfo var)
  -> Map Text (CIDRSet var)
  -> Graph (List ConnectionType) Reference
  -> TestEff (AdjacencyMap (List ConnectionType) C4Container)
toAdjacencyMap serviceIndex cidrIndex graph = do
  graphEdges <- traverse convertEdge (Graph.edgeList graph)
  pure (AM.edges graphEdges)
  where
    convertEdge (es, a, b) = do
      a' <- assertRight "Unexpected unknown puml type" $ toC4Container serviceIndex cidrIndex a
      b' <- assertRight "Unexpected unknown puml type" $ toC4Container serviceIndex cidrIndex b
      pure (es, a', b')

renderPumlType :: IO LazyByteString
renderPumlType = runTestEff $ do
  declarations <- partitionDeclarations <$> assertParseDocument "test/fixtures/puml-type.kdl"
  let aggregatedInventory = AggregatedInventory mempty mempty
  serviceDefinitions <- traverse (resolveServiceVars aggregatedInventory) declarations.services
  let graph = buildGraph serviceDefinitions declarations.entities
  let serviceIndex = buildServiceIndex serviceDefinitions
  cidrDefinitions <- traverse (resolveCIDRVars aggregatedInventory) declarations.cidrs
  let cidrIndex = buildCidrIndex cidrDefinitions
  void . assertRight "Graph is invalid" $ validationToEither (checkGraph graph)
  adjacencyMap <- toAdjacencyMap serviceIndex cidrIndex graph
  (pure . TL.encodeUtf8) . T.fromStrict $ Puml.renderPuml adjacencyMap

renderConnectOnly :: IO LazyByteString
renderConnectOnly = runTestEff $ do
  declarations <- partitionDeclarations <$> assertParseDocument "test/fixtures/connect-only.kdl"
  let aggregatedInventory = AggregatedInventory mempty mempty
  serviceDefinitions <- traverse (resolveServiceVars aggregatedInventory) declarations.services
  let graph = buildGraph serviceDefinitions declarations.entities
  let serviceIndex = buildServiceIndex serviceDefinitions
  void . assertRight "Graph is invalid" $ validationToEither (checkGraph graph)
  cidrDefinitions <- traverse (resolveCIDRVars aggregatedInventory) declarations.cidrs
  let cidrIndex = buildCidrIndex cidrDefinitions
  adjacencyMap <- toAdjacencyMap serviceIndex cidrIndex graph
  (pure . TL.encodeUtf8) . T.fromStrict $ Puml.renderPuml adjacencyMap

renderWebAppToBackend :: IO LazyByteString
renderWebAppToBackend = runTestEff $ do
  declarations <- partitionDeclarations <$> assertParseDocument "test/fixtures/web-app-to-backend.kdl"
  let aggregatedInventory = AggregatedInventory mempty mempty
  serviceDefinitions <- traverse (resolveServiceVars aggregatedInventory) declarations.services
  let graph = buildGraph serviceDefinitions declarations.entities
  let serviceIndex = buildServiceIndex serviceDefinitions
  cidrDefinitions <- traverse (resolveCIDRVars aggregatedInventory) declarations.cidrs
  let cidrIndex = buildCidrIndex cidrDefinitions
  void . assertRight "Graph is invalid" $ validationToEither (checkGraph graph)
  adjacencyMap <- toAdjacencyMap serviceIndex cidrIndex graph
  (pure . TL.encodeUtf8) . T.fromStrict $ Puml.renderPuml adjacencyMap

renderServices :: IO LazyByteString
renderServices = runTestEff $ do
  declarations <- partitionDeclarations <$> assertParseDocument "test/fixtures/multiple-service-definitions.kdl"
  let aggregatedInventory = AggregatedInventory mempty mempty
  serviceDefinitions <- traverse (resolveServiceVars aggregatedInventory) declarations.services
  let graph = buildGraph serviceDefinitions declarations.entities
  let serviceIndex = buildServiceIndex serviceDefinitions
  cidrDefinitions <- traverse (resolveCIDRVars aggregatedInventory) declarations.cidrs
  let cidrIndex = buildCidrIndex cidrDefinitions
  void . assertRight "Graph is invalid" $ validationToEither (checkGraph graph)
  adjacencyMap <- toAdjacencyMap serviceIndex cidrIndex graph
  (pure . TL.encodeUtf8) . T.fromStrict $ Puml.renderPuml adjacencyMap

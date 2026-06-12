{-# OPTIONS_GHC -Wno-x-test-only #-}

module Test.Render.PumlTests where

import Algebra.Graph.Labelled (Graph)
import Algebra.Graph.Labelled qualified as Graph
import Algebra.Graph.Labelled.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Data.ByteString.Lazy (LazyByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Data.Void (Void)
import Effectful.Error.Static qualified as Error
import Optics.Core
import Prettyprinter
import Prettyprinter.Render.Text
import Test.Tasty
import Test.Tasty.Golden
import Validation

import CLI.Error (CLIError)
import Core.Graph
import Core.Model.CIDRSet
import Core.Model.ContextName (ContextName)
import Core.Model.Inventory.Aggregated
import Core.Model.Reference
import Core.Model.Service
import Core.Model.ServiceContext qualified as ServiceContext
import Core.Model.ServiceName
import Core.Validation
import Driver.Puml
import Driver.Variable
import Parser.V1.Types
import Render.Puml (prettyContainerNode)
import Render.Puml qualified as Puml
import Render.Puml.C4Container.Types
import Render.Puml.PumlType
import Test.Utils

test :: TestTree
test =
  testGroup
    "PlantUML Renderer"
    [ testGroup
        "Container type"
        [ testThat "Containers are pretty-printed according to type and internal/external status" testContainerPrettyPrinting
        ]
    , testGroup
        "Golden Tests"
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
            "Nested contexts render as nested boundaries"
            diffCmd
            "test/golden/nested-contexts.puml"
            renderNestedContexts
        , testGroup
            "Renderer Props"
            [ goldenVsStringDiff
                "Service and CIDR Set with puml:type renders Container, ContainerDb and ContainerQueue"
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
        ]
    ]

testContainerPrettyPrinting :: TestEff Unit
testContainerPrettyPrinting = do
  let cases =
        [ ("service", ["k8s"], PumlService, "Container(service, \"service\")")
        , ("service", [], PumlService, "Container_Ext(service, \"service\")")
        , ("database", ["k8s"], PumlDatabase, "ContainerDb(database, \"database\")")
        , ("database", [], PumlDatabase, "ContainerDb_Ext(database, \"database\")")
        ]
  for_ cases $ \(name, hierarchy, pumlType, expected) -> do
    let container = C4Container {alias = mkC4ContainerAlias name, name, hierarchy, pumlType}
    assertEqual
      "Container type mismatch"
      expected
      (renderStrict . layoutPretty defaultLayoutOptions $ prettyContainerNode container)

testIsolatedUnknownPumlType :: TestEff Unit
testIsolatedUnknownPumlType = do
  let serviceInfo = emptyServiceInfo & #rendererProps .~ Map.fromList [("puml:type", "invalid")]
  let serviceIndex = Map.fromList [(ServiceName "postgres", serviceInfo)] :: Map ServiceName (ServiceInfo Void)
  let cidrIndex = Map.empty
  result <- Error.runErrorNoCallStack @(NonEmpty CLIError) $ validateContainers mempty serviceIndex cidrIndex []
  void $ assertLeft "Expected the isolated service to be rejected" result

testUnknownPumlType :: TestEff Unit
testUnknownPumlType = do
  let serviceInfo = emptyServiceInfo & #rendererProps .~ Map.fromList [("puml:type", "invalid")]
  let serviceIndex = Map.fromList [(ServiceName "postgres", serviceInfo)]
  let cidrIndex = Map.empty
  result <- assertLeft "Expected an unknown puml type error" $ toC4Container mempty serviceIndex cidrIndex (ServiceRef (ServiceName "postgres"))
  assertEqual
    "Expected the offending puml type to be reported"
    (InvalidPumlProp InvalidPumlPropError {name = "postgres", propKey = "puml:type", providedValue = "invalid", supportedValues = ["database", "queue", "service"]})
    result

testUnknownPumlProp :: TestEff Unit
testUnknownPumlProp = do
  let serviceInfo = emptyServiceInfo & #rendererProps .~ Map.fromList [("puml:tpye", "database")]
  let serviceIndex = Map.fromList [(ServiceName "postgres", serviceInfo)]
  let cidrIndex = Map.empty
  result <- assertLeft "Expected an unknown puml prop error" $ toC4Container mempty serviceIndex cidrIndex (ServiceRef (ServiceName "postgres"))
  assertEqual
    "Expected the offending puml prop key to be reported"
    (UnknownPumlProp UnknownPumlPropError {name = "postgres", propKey = "puml:tpye"})
    result

toAdjacencyMap
  :: Map ContextName (List ContextName)
  -> Map ServiceName (ServiceInfo var)
  -> Map Text (CIDRSet var)
  -> Graph (List ConnectionType) Reference
  -> TestEff (AdjacencyMap (List ConnectionType) C4Container)
toAdjacencyMap contextHierarchies serviceIndex cidrIndex graph = do
  graphEdges <- traverse convertEdge (Graph.edgeList graph)
  pure (AM.edges graphEdges)
  where
    convertEdge (es, a, b) = do
      a' <- assertRight "Unexpected unknown puml type" $ toC4Container contextHierarchies serviceIndex cidrIndex a
      b' <- assertRight "Unexpected unknown puml type" $ toC4Container contextHierarchies serviceIndex cidrIndex b
      pure (es, a', b')

-- | Parse a fixture, build the graph and indices, and render it to PlantUML.
renderFixture :: FilePath -> IO LazyByteString
renderFixture fixturePath = runTestEff $ do
  declarations <- partitionDeclarations <$> assertParseDocument fixturePath
  let aggregatedInventory = AggregatedInventory mempty mempty
  serviceDefinitions <- traverse (resolveServiceVars aggregatedInventory) declarations.services
  cidrDefinitions <- traverse (resolveCIDRVars aggregatedInventory) declarations.cidrs
  let graph = buildGraph serviceDefinitions declarations.entities
  let serviceIndex = buildServiceIndex serviceDefinitions
  let cidrIndex = buildCidrIndex cidrDefinitions
  void . assertRight "Graph is invalid" $ validationToEither (checkGraph graph)
  let contextHierarchies = ServiceContext.contextHierarchies declarations.contexts
  adjacencyMap <- toAdjacencyMap contextHierarchies serviceIndex cidrIndex graph
  (pure . TL.encodeUtf8) . T.fromStrict $ Puml.renderPuml adjacencyMap

renderPumlType :: IO LazyByteString
renderPumlType = renderFixture "test/fixtures/puml-type.kdl"

renderConnectOnly :: IO LazyByteString
renderConnectOnly = renderFixture "test/fixtures/connect-only.kdl"

renderNestedContexts :: IO LazyByteString
renderNestedContexts = renderFixture "test/fixtures/nested-contexts-puml.kdl"

renderWebAppToBackend :: IO LazyByteString
renderWebAppToBackend = renderFixture "test/fixtures/web-app-to-backend.kdl"

renderServices :: IO LazyByteString
renderServices = renderFixture "test/fixtures/multiple-service-definitions.kdl"

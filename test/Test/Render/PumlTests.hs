module Test.Render.PumlTests where

import Algebra.Graph.Labelled qualified as Graph
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Data.ByteString.Lazy (LazyByteString)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Test.Tasty
import Test.Tasty.Golden
import Validation

import Core.Graph
import Core.Model.Inventory.Aggregated
import Core.Validation
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
    ]

renderConnectOnly :: IO LazyByteString
renderConnectOnly = runTestEff $ do
  declarations <- partitionDeclarations <$> assertParseDocument "test/fixtures/connect-only.kdl"
  let entities = declarations.entities
  let aggregatedInventory = AggregatedInventory mempty mempty
  serviceDefinitions <- traverse (resolveServiceVars aggregatedInventory) declarations.services
  let graph = buildGraph serviceDefinitions entities
  let serviceIndex = buildServiceIndex serviceDefinitions
  void . assertRight "Graph is invalid" $ validationToEither (checkGraph graph)
  liftIO $ print graph
  let graphEdges =
        graph
          & Graph.edgeList
          & fmap (\(es, a, b) -> (es, toC4Container serviceIndex a, toC4Container serviceIndex b))
  let adjacencyMap = AM.edges graphEdges
  (pure . TL.encodeUtf8) . T.fromStrict $ Puml.renderPuml adjacencyMap

renderWebAppToBackend :: IO LazyByteString
renderWebAppToBackend = runTestEff $ do
  declarations <- partitionDeclarations <$> assertParseDocument "test/fixtures/web-app-to-backend.kdl"
  let entities = declarations.entities
  let aggregatedInventory = AggregatedInventory mempty mempty
  serviceDefinitions <- traverse (resolveServiceVars aggregatedInventory) declarations.services
  let graph = buildGraph serviceDefinitions entities
  let serviceIndex = buildServiceIndex serviceDefinitions
  void . assertRight "Graph is invalid" $ validationToEither (checkGraph graph)
  let graphEdges =
        graph
          & Graph.edgeList
          & fmap (\(es, a, b) -> (es, toC4Container serviceIndex a, toC4Container serviceIndex b))
  let adjacencyMap = AM.edges graphEdges
  (pure . TL.encodeUtf8) . T.fromStrict $ Puml.renderPuml adjacencyMap

renderServices :: IO LazyByteString
renderServices = runTestEff $ do
  declarations <- partitionDeclarations <$> assertParseDocument "test/fixtures/multiple-service-definitions.kdl"
  let entities = declarations.entities
  let aggregatedInventory = AggregatedInventory mempty mempty
  serviceDefinitions <- traverse (resolveServiceVars aggregatedInventory) declarations.services
  let graph = buildGraph serviceDefinitions entities
  let serviceIndex = buildServiceIndex serviceDefinitions
  void . assertRight "Graph is invalid" $ validationToEither (checkGraph graph)
  let graphEdges =
        graph
          & Graph.edgeList
          & fmap (\(es, a, b) -> (es, toC4Container serviceIndex a, toC4Container serviceIndex b))
  let adjacencyMap = AM.edges graphEdges
  (pure . TL.encodeUtf8) . T.fromStrict $ Puml.renderPuml adjacencyMap

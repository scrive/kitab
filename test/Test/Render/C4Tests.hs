module Test.Render.C4Tests where

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
import Render.C4 qualified as C4
import Render.C4.C4Container.Types
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
    ]

renderWebAppToBackend :: IO LazyByteString
renderWebAppToBackend = runTestEff $ do
  declarations <- assertParseDocument "test/fixtures/web-app-to-backend.kdl"
  let serviceDefinitions' =
        mapMaybe
          ( \case
              ServiceDeclaration s -> Just s
              _ -> Nothing
          )
          declarations
  let entities =
        mapMaybe
          ( \case
              EntityDeclaration c -> Just c
              _ -> Nothing
          )
          declarations
  let aggregatedInventory = AggregatedInventory mempty mempty
  serviceDefinitions <- traverse (resolveServiceVars aggregatedInventory) serviceDefinitions'
  let graph = buildGraph serviceDefinitions entities
  let serviceIndex = buildServiceIndex serviceDefinitions
  void . assertRight "Graph is invalid" $ validationToEither (checkGraph graph)
  let graphEdges =
        graph
          & Graph.edgeList
          & fmap (\(es, a, b) -> (es, toC4Container serviceIndex a, toC4Container serviceIndex b))
  let adjacencyMap = AM.edges graphEdges
  (pure . TL.encodeUtf8) . T.fromStrict $ C4.renderC4 adjacencyMap

renderServices :: IO LazyByteString
renderServices = runTestEff $ do
  declarations <- assertParseDocument "test/fixtures/multiple-service-definitions.kdl"
  let serviceDefinitions' =
        mapMaybe
          ( \case
              ServiceDeclaration s -> Just s
              _ -> Nothing
          )
          declarations
  let entities =
        mapMaybe
          ( \case
              EntityDeclaration c -> Just c
              _ -> Nothing
          )
          declarations
  let aggregatedInventory = AggregatedInventory mempty mempty
  serviceDefinitions <- traverse (resolveServiceVars aggregatedInventory) serviceDefinitions'
  let graph = buildGraph serviceDefinitions entities
  let serviceIndex = buildServiceIndex serviceDefinitions
  void . assertRight "Graph is invalid" $ validationToEither (checkGraph graph)
  let graphEdges =
        graph
          & Graph.edgeList
          & fmap (\(es, a, b) -> (es, toC4Container serviceIndex a, toC4Container serviceIndex b))
  let adjacencyMap = AM.edges graphEdges
  (pure . TL.encodeUtf8) . T.fromStrict $ C4.renderC4 adjacencyMap

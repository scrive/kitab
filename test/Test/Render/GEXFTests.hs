module Test.Render.GEXFTests where

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
import Render.GEXF qualified as GEXF
import Test.Utils

test :: TestTree
test =
  testGroup
    "GEXF rendering golden tests"
    [ goldenVsStringDiff
        "Services"
        diffCmd
        "test/golden/services.gexf"
        renderServices
    ]

renderServices :: IO LazyByteString
renderServices = runTestEff $ do
  declarations <- partitionDeclarations <$> assertParseDocument "test/fixtures/multiple-service-definitions.kdl"
  let entities = declarations.entities
  let aggregatedInventory = AggregatedInventory mempty mempty
  serviceDefinitions <- traverse (resolveServiceVars aggregatedInventory) declarations.services
  let graph = buildGraph serviceDefinitions entities
  let serviceIndex = buildServiceIndex serviceDefinitions
  void . assertRight "Graph is invalid" $ validationToEither (checkGraph graph)
  let adjacencyMap =
        graph
          & Graph.edgeList
          & AM.edges
  (pure . TL.encodeUtf8) . T.fromStrict $ GEXF.renderToGEXF adjacencyMap serviceIndex

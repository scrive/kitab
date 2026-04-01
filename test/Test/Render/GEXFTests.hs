module Test.Render.GEXFTests where

import Algebra.Graph.Labelled qualified as Graph
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Data.ByteString.Lazy (LazyByteString)
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import Test.Tasty
import Test.Tasty.Golden
import Validation

import Core.Graph
import Core.Model.Inventory.Aggregated
import Core.Validation
import Driver.Variable
import Parser
import Parser.Types
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
  fileContent <- T.decodeUtf8 <$> FileSystem.readFile "test/fixtures/multiple-service-definitions.kdl"
  declarations <- assertParse decodeServiceDocument fileContent
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
  let adjacencyMap =
        graph
          & Graph.edgeList
          & AM.edges
  (pure . TL.encodeUtf8) . T.fromStrict $ GEXF.renderGEXF adjacencyMap serviceIndex

module Test.Render.C4Tests where

import Algebra.Graph.Labelled qualified as Graph
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Data.ByteString.Lazy (LazyByteString)
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import KDL qualified
import Test.Tasty
import Test.Tasty.Golden
import Validation

import Core.Graph
import Core.Validation
import Parser
import Parser.Types
import Render.C4 qualified as C4
import Render.C4.C4Service.Types
import Test.Utils

test :: TestTree
test =
  testGroup
    "C4 rendering golden test"
    [ goldenVsStringDiff
        "Services"
        diffCmd
        "test/golden/service.puml"
        renderServices
    ]

renderServices :: IO LazyByteString
renderServices = runTestEff $ do
  fileContent <- T.decodeUtf8 <$> FileSystem.readFile "test/fixtures/services/multiple-service-definitions.kdl"
  declarations <- assertRight "KDL file could not be parsed" $ KDL.decodeWith decodeServiceDocument fileContent
  let serviceDefinitions =
        mapMaybe
          ( \case
              ServiceDeclaration s -> Just s
              _ -> Nothing
          )
          declarations
  let contexts =
        mapMaybe
          ( \case
              ContextDeclaration c -> Just c
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
  let graph = buildGraph serviceDefinitions entities
  let serviceIndex = buildServiceIndex serviceDefinitions
  void . assertRight "Graph is invalid" $ validationToEither (checkGraph graph)
  let graphEdges =
        graph
          & Graph.edgeList
          & fmap (\(es, a, b) -> (es, toC4Service serviceIndex a, toC4Service serviceIndex b))
  let adjacencyMap = AM.edges graphEdges
  (pure . TL.encodeUtf8) . T.fromStrict $ C4.renderC4 contexts adjacencyMap

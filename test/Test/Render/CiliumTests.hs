module Test.Render.CiliumTests where

import Algebra.Graph.Labelled qualified as Graph
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Data.ByteString.Lazy (LazyByteString)
import Data.List qualified as List
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import KDL qualified
import Test.Tasty
import Test.Tasty.Golden
import Validation

import Core.Model
import Core.Validation
import Parser
import Render.Cilium qualified as Cilium
import Render.Cilium.Types
import Test.Utils

diffCmd :: String -> String -> [String]
diffCmd ref new = ["diff", "-u", ref, new]

test :: TestTree
test =
  testGroup
    "Cilium rendering golden test"
    [ goldenVsStringDiff
        "Services"
        diffCmd
        "test/golden/network-policy.yaml"
        renderService
    ]

renderService :: IO LazyByteString
renderService = runTestEff $ do
  fileContent <- T.decodeUtf8 <$> FileSystem.readFile "test/fixtures/media-proxy.kdl"
  serviceDefinitions <- assertRight "KDL file could not be parsed" $ KDL.decodeWith decodeServiceDocument fileContent
  let graph = build serviceDefinitions
  let serviceIndex = buildIndex serviceDefinitions
  void . assertRight "Graph is invalid" $ validationToEither (checkGraph graph)
  mediaProxyService <- assertJust "" $ List.find (\s -> s.serviceName == "media-proxy") serviceDefinitions
  ((pure . TL.encodeUtf8) . T.fromStrict) . Cilium.renderCilium $ Cilium.toCiliumPolicy serviceIndex mediaProxyService

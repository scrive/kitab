module Test.Render.CiliumTests where

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

import Core.Graph
import Core.Model.Service
import Core.Validation
import Parser
import Parser.Types
import Render.Cilium qualified as Cilium
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
    ]

renderService :: IO LazyByteString
renderService = runTestEff $ do
  fileContent <- T.decodeUtf8 <$> FileSystem.readFile "test/fixtures/multiple-service-definitions.kdl"
  declarations <- assertRight "KDL file could not be parsed" $ KDL.decodeWith decodeServiceDocument fileContent
  let serviceDefinitions =
        mapMaybe
          ( \case
              ServiceDeclaration s -> Just s
              _ -> Nothing
          )
          declarations

  let entities =
        mapMaybe
          ( \case
              EntityDeclaration s -> Just s
              _ -> Nothing
          )
          declarations

  let graph = buildGraph serviceDefinitions entities
  let serviceIndex = buildServiceIndex serviceDefinitions
  let entityIndex = buildEntityIndex entities
  void . assertRight "Graph is invalid" $ validationToEither (checkGraph graph)
  mediaProxyService <- assertJust "" $ List.find (\s -> s.serviceName == "media-proxy") serviceDefinitions
  ((pure . TL.encodeUtf8) . T.fromStrict) . Cilium.renderCilium $ Cilium.toCiliumPolicy serviceIndex entityIndex mediaProxyService

renderCIDRSetPolicy :: IO LazyByteString
renderCIDRSetPolicy = runTestEff $ do
  fileContent <- T.decodeUtf8 <$> FileSystem.readFile "test/fixtures/cidrset.kdl"
  declarations <- assertRight "KDL file could not be parsed" $ KDL.decodeWith decodeServiceDocument fileContent
  let serviceDefinitions =
        mapMaybe
          ( \case
              ServiceDeclaration s -> Just s
              _ -> Nothing
          )
          declarations
  let entities =
        mapMaybe
          ( \case
              EntityDeclaration s -> Just s
              _ -> Nothing
          )
          declarations

  let graph = buildGraph serviceDefinitions entities
  let serviceIndex = buildServiceIndex serviceDefinitions
  let entityIndex = buildEntityIndex entities
  void . assertRight "Graph is invalid" $ validationToEither (checkGraph graph)
  myAppService <- assertJust "" $ List.find (\s -> s.serviceName == "my-app") serviceDefinitions
  ((pure . TL.encodeUtf8) . T.fromStrict) . Cilium.renderCilium $ Cilium.toCiliumPolicy serviceIndex entityIndex myAppService

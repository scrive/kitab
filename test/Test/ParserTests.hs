{-# LANGUAGE OverloadedLists #-}

module Test.ParserTests where

import Algebra.Graph.Export.Dot
import Data.ByteString.Lazy (LazyByteString)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import KDL qualified
import Test.Tasty
import Test.Tasty.Golden
import Text.Pretty.Simple

import Core.Graph
import Core.Model.Inventory
import Core.Model.InventoryVariable
import Core.Model.PortNode
import Core.Model.Service
import Core.Model.ServiceName
import Core.Variable
import Parser
import Parser.Inventory (inventoryDecoder)
import Parser.Service
import Parser.Types
import Test.Utils

test :: TestTree
test =
  testGroup
    "Parser"
    [ testThat "Decode single service definition" testServiceDecoding
    , testGroup
        "Golden"
        [ goldenVsStringDiff
            "Haskell AST"
            diffCmd
            "test/golden/parsed-multiple-services.hs"
            testServiceDefinitionsParsing
        , goldenVsStringDiff
            "DOT representation"
            diffCmd
            "test/golden/multiple-definitions.dot"
            testGraphToDot
        ]
    , testGroup
        "Inventory"
        [ testThat
            "Decode inventory"
            testInventoryDecoding
        , testThat
            "Service with variable"
            testParsingServiceWithVar
        ]
    ]

testServiceDecoding :: TestEff ()
testServiceDecoding = do
  serviceDefinition <- decodeUtf8 <$> FileSystem.readFile "test/fixtures/service-definition.kdl"
  let expectedResult = Service {serviceName = "media-proxy", serviceInfo = defaultServiceInfo, entityAccesses = [], serviceConnections = [Connection {connectionWith = ServiceName "main-app", connectionType = HTTPS, connectionPorts = [PortNode 3833 "TCP"]}], cidrSets = []}
  result <- assertParse (KDL.document serviceDecoder) serviceDefinition
  assertEqual
    "Expected service definition"
    expectedResult
    result

testServiceDefinitionsParsing :: IO LazyByteString
testServiceDefinitionsParsing = runTestEff $ do
  serviceDefinition <- decodeUtf8 <$> FileSystem.readFile "test/fixtures/multiple-service-definitions.kdl"
  declarations <- assertParse decodeServiceDocument serviceDefinition
  pure . TL.encodeUtf8 $ pShowNoColor declarations

testGraphToDot :: IO LazyByteString
testGraphToDot = runTestEff $ do
  serviceDefinition <- decodeUtf8 <$> FileSystem.readFile "test/fixtures/multiple-service-definitions.kdl"
  declarations <- assertParse decodeServiceDocument serviceDefinition
  let entities =
        mapMaybe
          ( \case
              EntityDeclaration e -> Just e
              _ -> Nothing
          )
          declarations
  let serviceDefinitions' =
        mapMaybe
          ( \case
              ServiceDeclaration s -> Just s
              _ -> Nothing
          )
          declarations

  let graph = buildGraph serviceDefinitions' entities

  pure . TL.encodeUtf8 $ TL.fromStrict (export (defaultStyle display) graph)

testInventoryDecoding :: TestEff ()
testInventoryDecoding = do
  inventoryDefinition <- decodeUtf8 <$> FileSystem.readFile "test/fixtures/inventory.kdl"
  let expectedResult = Inventory {name = "base", vars = [("opensearch-fqdn", InventoryVariable {name = "opensearch-fqdn", value = "opensearch.aws.internal.network", description = Just "OpenSearch instance in AWS"})]}
  result <- assertParse (KDL.document inventoryDecoder) inventoryDefinition
  assertEqual
    "Expected inventory definition"
    expectedResult
    result

testParsingServiceWithVar :: TestEff ()
testParsingServiceWithVar = do
  serviceDefinition <- decodeUtf8 <$> FileSystem.readFile "test/fixtures/service-with-var.kdl"
  let expectedResult =
        Service
          { serviceName = "opensearch"
          , serviceInfo = ServiceInfo {serviceFqdn = Just (Left (Var "opensearch-fqdn")), serviceContext = Nothing, servicePorts = []}
          , serviceConnections = []
          , cidrSets = []
          , entityAccesses = []
          }
  result <- assertParse (KDL.document serviceDecoder) serviceDefinition
  assertEqual
    "Expected service definition"
    expectedResult
    result

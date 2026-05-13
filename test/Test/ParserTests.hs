{-# LANGUAGE OverloadedLists #-}

module Test.ParserTests where

import Algebra.Graph.Export.Dot
import Data.ByteString.Lazy (LazyByteString)
import Data.Map.Strict qualified as Map
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import KDL
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
import Parser.V1.Inventory (inventoryDecoder)
import Parser.V1.Service
import Parser.V1.Types
import Test.Utils

test :: TestTree
test =
  testGroup
    "Parser"
    [ testThat "Decode single service definition" testServiceDecoding
    , testGroup
        "Version"
        [ testThat "Unknown version number is handled" testUnkonwnVersionHandling
        , testThat "Invalid version value is handled" testInvalidVersionHandling
        ]
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
  let expectedResult =
        emptyService
          { serviceName = "media-proxy"
          , serviceInfo = defaultServiceInfo
          , serviceConnections =
              [ Connection {connectionWith = ServiceName "main-app", connectionType = Network, connectionPorts = [PortNode 3833 "TCP"]}
              ]
          }
  result <- assertParse (KDL.document serviceDecoder) "test/fixtures/service-definition.kdl"
  assertEqual
    "Expected service definition"
    expectedResult
    result

testUnkonwnVersionHandling :: TestEff ()
testUnkonwnVersionHandling = do
  assertParseError
    "./test/fixtures/unknown-version.kdl"
    [ "At: <root>"
    , "  This kitab can only parse configuration format version 1. Found 3.0"
    ]

testInvalidVersionHandling :: TestEff ()
testInvalidVersionHandling = do
  assertParseError
    "./test/fixtures/invalid-version.kdl"
    ["At: <root>", "  Expected number, got: lol"]

testServiceDefinitionsParsing :: IO LazyByteString
testServiceDefinitionsParsing = runTestEff $ do
  declarations <- assertParseDocument "test/fixtures/multiple-service-definitions.kdl"
  pure . TL.encodeUtf8 $ pShowNoColor declarations

testGraphToDot :: IO LazyByteString
testGraphToDot = runTestEff $ do
  declarations <- assertParseDocument "test/fixtures/multiple-service-definitions.kdl"
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
  let expectedResult =
        emptyInventory
          { attributes = Map.fromList [("cloud", "aws"), ("env", "prod"), ("region", "eu-west-1")]
          , vars =
              [ ("mysql-cluster-cidr", InventoryVariable {name = "mysql-cluster-cidr", value = "10.147.128.0/24", description = Just "MySQL cluster"})
              , ("opensearch-fqdn", InventoryVariable {name = "opensearch-fqdn", value = "opensearch.aws.internal.network", description = Just "OpenSearch instance in AWS"})
              , ("otel-tracing-fqdn", InventoryVariable {name = "otel-tracing-fqdn", value = "otel.aws.internal.network", description = Just "OpenTelemetry ingestion in AWS"})
              ]
          }
  result <- assertParse (KDL.document inventoryDecoder) "test/fixtures/inventory.kdl"
  assertEqual
    "Expected inventory definition"
    expectedResult
    result

testParsingServiceWithVar :: TestEff ()
testParsingServiceWithVar = do
  let expectedResult =
        emptyService
          { serviceName = "opensearch"
          , serviceInfo = defaultServiceInfo {serviceFqdn = Just (Left (Var "opensearch-fqdn"))}
          , serviceConnections =
              []
          }
  result <- assertParse (KDL.document serviceDecoder) "test/fixtures/service-with-var.kdl"
  assertEqual
    "Expected service definition"
    expectedResult
    result

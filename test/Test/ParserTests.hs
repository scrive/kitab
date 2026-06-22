{-# LANGUAGE OverloadedLists #-}

module Test.ParserTests (test) where

import Algebra.Graph.Export.Dot
import Data.ByteString.Lazy (LazyByteString)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import KDL
import Test.Tasty
import Test.Tasty.Golden
import Text.Pretty.Simple

import Core.Graph
import Core.Model.CIDRSet
import Core.Model.Entity
import Core.Model.Inventory
import Core.Model.InventoryVariable
import Core.Model.PortNode
import Core.Model.Service
import Core.Model.ServiceContext
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
        "Metadata"
        [ testThat "Invalid service prop values are parsed" testInvalidPropValuesAreParsed
        , testThat "Invalid service prop are rejected" testInvalidPropKeysAreParsed
        ]
    , testGroup
        "Version"
        [ testThat "Unknown version number is handled" testUnkonwnVersionHandling
        , testThat "Invalid version value is handled" testInvalidVersionHandling
        ]
    , testGroup
        "Connections"
        [ testThat "Unknown connection type is rejected" testUnknownConnectionType
        , testThat "Connection without via node is rejected" testMissingViaNode
        ]
    , testGroup
        "Declarations"
        [ testThat "partitionDeclarations sorts into buckets" testPartitionDeclarations
        , testThat "partitionDeclarations discards version and tool" testPartitionDiscards
        , testThat "partitionDeclarations preserves order" testPartitionOrder
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

testServiceDecoding :: TestEff Unit
testServiceDecoding = do
  let expectedResult =
        emptyService
          { serviceName = "media-proxy"
          , serviceInfo = emptyServiceInfo
          , serviceConnections =
              [ Connection {connectionWith = ServiceName "main-app", connectionType = HTTPS, connectionPorts = [PortNode 3833 "TCP"]}
              ]
          }
  result <- assertParse (KDL.document serviceDecoder) "test/fixtures/service-definition.kdl"
  assertEqual
    "Expected service definition"
    expectedResult
    result

testInvalidPropValuesAreParsed :: TestEff Unit
testInvalidPropValuesAreParsed = do
  result <- assertParse (KDL.document serviceDecoder) "test/fixtures/invalid-service-prop-value.kdl"
  assertEqual
    "Could not parse invalid prop value"
    (Map.fromList [("puml:type", "invalid")])
    result.serviceInfo.rendererProps

testInvalidPropKeysAreParsed :: TestEff Unit
testInvalidPropKeysAreParsed = do
  result <- assertParse (KDL.document serviceDecoder) "test/fixtures/invalid-service-prop-key.kdl"
  assertEqual
    "Could not parse invalid prop key"
    (Map.fromList [("puml:blargh", "database")])
    result.serviceInfo.rendererProps

testUnkonwnVersionHandling :: TestEff Unit
testUnkonwnVersionHandling = do
  assertParseError
    "./test/fixtures/unknown-version.kdl"
    [ "At: <root>"
    , "  This kitab can only parse configuration format version 1. Found 3.0"
    ]

testInvalidVersionHandling :: TestEff Unit
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
  declarations <- partitionDeclarations <$> assertParseDocument "test/fixtures/multiple-service-definitions.kdl"
  let graph = buildGraph declarations.services declarations.entities

  pure . TL.encodeUtf8 $ TL.fromStrict (export (defaultStyle display) graph)

testInventoryDecoding :: TestEff Unit
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

testParsingServiceWithVar :: TestEff Unit
testParsingServiceWithVar = do
  let expectedResult =
        emptyService
          { serviceName = "opensearch"
          , serviceInfo = emptyServiceInfo {serviceFqdn = Just (Left (Var "opensearch-fqdn"))}
          , serviceConnections =
              []
          }
  result <- assertParse (KDL.document serviceDecoder) "test/fixtures/service-with-var.kdl"
  assertEqual
    "Expected service definition"
    expectedResult
    result

testUnknownConnectionType :: TestEff Unit
testUnknownConnectionType = do
  rendered <- assertDecodeError (KDL.document serviceDecoder) "test/fixtures/unknown-connection-type.kdl"
  assertBool
    "Error mentions the unknown connection type"
    (T.isInfixOf "unknown connection type" rendered)

testMissingViaNode :: TestEff Unit
testMissingViaNode = do
  void $ assertDecodeError (KDL.document serviceDecoder) "test/fixtures/missing-via.kdl"

testPartitionDeclarations :: TestEff Unit
testPartitionDeclarations = do
  let serviceA = emptyService {serviceName = "A"}
      serviceB = emptyService {serviceName = "B"}
      sampleEntity = Entity {entityName = "ext", entityInfo = EntityInfo {entityPorts = Set.empty, entityContext = Nothing}}
      sampleContext = ServiceContext {contextName = "ctx", subContexts = []}
      sampleCidrSet =
        CIDRSet
          { setName = "cs"
          , cidrRules = NE.singleton (CidrRuleNode {cidr = Right ("10.0.0.0/8", Nothing), excepts = []})
          , ports = []
          , context = Nothing
          , rendererProps = Map.empty
          }
      sampleDeclarations =
        [ ServiceDeclaration serviceA
        , EntityDeclaration sampleEntity
        , ContextDeclaration sampleContext
        , CIDRSetDeclaration sampleCidrSet
        , VersionDeclaration 1
        , ToolDeclaration "kitab"
        , ServiceDeclaration serviceB
        ]
  let result = partitionDeclarations sampleDeclarations
  assertEqual "Services bucket" [serviceA, serviceB] result.services
  assertEqual "Entities bucket" [sampleEntity] result.entities
  assertEqual "Contexts bucket" [sampleContext] result.contexts
  assertEqual "CIDR sets bucket" [sampleCidrSet] result.cidrs

-- VersionDeclaration and ToolDeclaration have no bucket; partitioning a list
-- that contains only those must yield an empty Declarations.
testPartitionDiscards :: TestEff Unit
testPartitionDiscards = do
  let result = partitionDeclarations [VersionDeclaration 1, ToolDeclaration "kitab"]
  assertEqual "No services" [] result.services
  assertEqual "No entities" [] result.entities
  assertEqual "No contexts" [] result.contexts
  assertEqual "No CIDR sets" [] result.cidrs

testPartitionOrder :: TestEff Unit
testPartitionOrder = do
  let serviceA = emptyService {serviceName = "A"}
      serviceB = emptyService {serviceName = "B"}
  let result = partitionDeclarations [ServiceDeclaration serviceA, ServiceDeclaration serviceB]
  assertEqual "Declaration order is preserved" [serviceA, serviceB] result.services

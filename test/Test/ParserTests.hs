module Test.ParserTests where

import Data.Text.Encoding (decodeUtf8)
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import KDL qualified
import Test.Tasty

import Core.Model
import Parser
import Test.Utils

test :: TestTree
test =
  testGroup
    "Parser"
    [ testThat "Decode single service definition" testServiceDecoding
    , testThat "Decode document with many service definitions" testServiceDefinitionsParsing
    ]

testServiceDecoding :: TestEff ()
testServiceDecoding = do
  serviceDefinition <- decodeUtf8 <$> FileSystem.readFile "test/fixtures/service-definition.kdl"
  let expectedResult = Service {serviceName = "media-proxy", serviceFqdn = Nothing, connections = [Connection {connectionWith = "main-app", connectionType = HTTPS}]}
  result <- assertRight "KDL file could not be parsed" $ KDL.decodeWith decodeService serviceDefinition
  assertEqual
    "Expected service definition"
    expectedResult
    result

testServiceDefinitionsParsing :: TestEff ()
testServiceDefinitionsParsing = do
  serviceDefinition <- decodeUtf8 <$> FileSystem.readFile "test/fixtures/multiple-service-definitions.kdl"
  let expectedResult =
        [ Service {serviceName = "otel-tracing", serviceFqdn = Just "tracing.internal.network", connections = []}
        , Service {serviceName = "opensearch", serviceFqdn = Just "opensearch.internal.network", connections = []}
        , Service {serviceName = "media-proxy", serviceFqdn = Nothing, connections = [Connection {connectionWith = "otel-tracing", connectionType = HTTPS}]}
        , Service {serviceName = "main-app", serviceFqdn = Nothing, connections = [Connection {connectionWith = "s3", connectionType = HTTPS}, Connection {connectionWith = "media-proxy", connectionType = HTTPS}, Connection {connectionWith = "user-registry", connectionType = FunctionCall}, Connection {connectionWith = "otel-tracing", connectionType = HTTPS}]}
        ]

  result <- assertRight "KDL file could not be parsed" $ KDL.decodeWith decodeServiceDocument serviceDefinition
  assertEqual
    "Expected service definition"
    expectedResult
    result

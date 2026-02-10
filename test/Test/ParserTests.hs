module Test.ParserTests where

import Data.Text.Encoding (decodeUtf8)
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import KDL qualified
import Test.Tasty

import Core.Model.Service
import Core.Model.ServiceContext
import Parser
import Parser.Service
import Parser.Types
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
  let expectedResult = Service {serviceName = "media-proxy", serviceInfo = defaultServiceInfo, connections = [Connection {connectionWith = "main-app", connectionType = HTTPS, connectionPorts = [3833]}], cidrSets = []}
  result <- assertRight "KDL file could not be parsed" $ KDL.decodeWith (KDL.document $ KDL.nodeWith "service" serviceDecoder) serviceDefinition
  assertEqual
    "Expected service definition"
    expectedResult
    result

testServiceDefinitionsParsing :: TestEff ()
testServiceDefinitionsParsing = do
  serviceDefinition <- decodeUtf8 <$> FileSystem.readFile "test/fixtures/multiple-service-definitions.kdl"
  let expectedResult =
        [ Service {serviceName = "otel-tracing", serviceInfo = ServiceInfo {serviceFqdn = Just "tracing.internal.network", serviceContext = Nothing}, connections = [], cidrSets = []}
        , Service {serviceName = "opensearch", serviceInfo = ServiceInfo {serviceFqdn = Just "opensearch.internal.network", serviceContext = Nothing}, connections = [], cidrSets = []}
        , Service {serviceName = "media-proxy", serviceInfo = ServiceInfo {serviceFqdn = Nothing, serviceContext = Just (ServiceContext {contextName = "k8s"})}, connections = [Connection {connectionWith = "opensearch", connectionType = HTTPS, connectionPorts = []}, Connection {connectionWith = "otel-tracing", connectionType = HTTPS, connectionPorts = [4317, 4318]}], cidrSets = []}
        , Service {serviceName = "user-registry", serviceInfo = ServiceInfo {serviceFqdn = Nothing, serviceContext = Just (ServiceContext {contextName = "k8s"})}, connections = [], cidrSets = []}
        , Service {serviceName = "main-app", serviceInfo = ServiceInfo {serviceFqdn = Nothing, serviceContext = Just (ServiceContext {contextName = "k8s"})}, connections = [Connection {connectionWith = "s3", connectionType = HTTPS, connectionPorts = []}, Connection {connectionWith = "media-proxy", connectionType = HTTPS, connectionPorts = []}, Connection {connectionWith = "user-registry", connectionType = FunctionCall, connectionPorts = []}, Connection {connectionWith = "otel-tracing", connectionType = HTTPS, connectionPorts = [4317]}], cidrSets = []}
        ]
  declarations <- assertRight "KDL file could not be parsed" $ KDL.decodeWith decodeServiceDocument serviceDefinition
  let result =
        mapMaybe
          ( \case
              ServiceDeclaration s -> Just s
              ContextDeclaration _ -> Nothing
          )
          declarations

  assertEqual
    "Expected service definition"
    expectedResult
    result

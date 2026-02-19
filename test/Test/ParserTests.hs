{-# LANGUAGE OverloadedLists #-}

module Test.ParserTests where

import Data.Text.Encoding (decodeUtf8)
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import KDL qualified
import Test.Tasty

import Core.Model.PortNode
import Core.Model.Service
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
  let expectedResult = Service {serviceName = "media-proxy", serviceInfo = defaultServiceInfo, connections = [Connection {connectionWith = ServiceReference "main-app" Nothing, connectionType = HTTPS, connectionPorts = [PortNode 3833 "TCP"]}], cidrSets = []}
  result <- assertRight "KDL file could not be parsed" $ KDL.decodeWith (KDL.document serviceDecoder) serviceDefinition
  assertEqual
    "Expected service definition"
    expectedResult
    result

testServiceDefinitionsParsing :: TestEff ()
testServiceDefinitionsParsing = do
  serviceDefinition <- decodeUtf8 <$> FileSystem.readFile "test/fixtures/multiple-service-definitions.kdl"
  let expectedResult =
        []
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

{-# LANGUAGE OverloadedLists #-}

module Test.ParserTests where

import Data.ByteString.Lazy (LazyByteString)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy.Encoding qualified as TL
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import KDL qualified
import Test.Tasty
import Test.Tasty.Golden
import Text.Pretty.Simple

import Core.Model.PortNode
import Core.Model.Service
import Core.Model.ServiceName
import Parser
import Parser.Service
import Test.Utils

test :: TestTree
test =
  testGroup
    "Parser"
    [ testThat "Decode single service definition" testServiceDecoding
    , testGroup
        "Golden"
        [ goldenVsStringDiff
            "Decode document with many service definitions"
            diffCmd
            "test/golden/parsed-multiple-services.hs"
            testServiceDefinitionsParsing
        ]
    ]

testServiceDecoding :: TestEff ()
testServiceDecoding = do
  serviceDefinition <- decodeUtf8 <$> FileSystem.readFile "test/fixtures/service-definition.kdl"
  let expectedResult = Service {serviceName = "media-proxy", serviceInfo = defaultServiceInfo, connections = [Connection {connectionWith = ServiceName "main-app", connectionType = HTTPS, connectionPorts = [PortNode 3833 "TCP"]}], cidrSets = []}
  result <- assertRight "KDL file could not be parsed" $ KDL.decodeWith (KDL.document serviceDecoder) serviceDefinition
  assertEqual
    "Expected service definition"
    expectedResult
    result

testServiceDefinitionsParsing :: IO LazyByteString
testServiceDefinitionsParsing = runTestEff $ do
  serviceDefinition <- decodeUtf8 <$> FileSystem.readFile "test/fixtures/multiple-service-definitions.kdl"
  declarations <- assertRight "KDL file could not be parsed" $ KDL.decodeWith decodeServiceDocument serviceDefinition
  pure . TL.encodeUtf8 $ pShowNoColor declarations

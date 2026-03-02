{-# LANGUAGE OverloadedLists #-}

module Test.ParserTests (test) where

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
import Core.Model.PortNode
import Core.Model.Service
import Core.Model.ServiceName
import Core.Model.Variable
import Parser
import Parser.Service
import Parser.Types
import Parser.Variable
import Test.Utils

test :: TestTree
test =
  testGroup
    "Parser"
    [ testGroup
        "Services"
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
        ]
    , testGroup
        "Variables"
        [ testThat
            "Parse a variable declaration"
            testVariableParsing
        , testThat
            "Parse a variable declaration without validation"
            testVariableWithoutValidationParsing
        , testThat "Values must be declared if the node is present" testVariableWithMissingValues
        ]
    ]

testServiceDecoding :: TestEff ()
testServiceDecoding = do
  serviceDefinition <- decodeUtf8 <$> FileSystem.readFile "test/fixtures/services/service-definition.kdl"
  let expectedResult = Service {serviceName = "media-proxy", serviceInfo = defaultServiceInfo, entityAccesses = [], serviceConnections = [Connection {connectionWith = ServiceName "main-app", connectionType = HTTPS, connectionPorts = [PortNode 3833 "TCP"]}], cidrSets = []}
  result <- assertRight "KDL file could not be parsed" $ KDL.decodeWith (KDL.document serviceDecoder) serviceDefinition
  assertEqual
    "Expected service definition"
    expectedResult
    result

testServiceDefinitionsParsing :: IO LazyByteString
testServiceDefinitionsParsing = runTestEff $ do
  serviceDefinition <- decodeUtf8 <$> FileSystem.readFile "test/fixtures/services/multiple-service-definitions.kdl"
  declarations <- assertRight "KDL file could not be parsed" $ KDL.decodeWith decodeServiceDocument serviceDefinition
  pure . TL.encodeUtf8 $ pShowNoColor declarations

testGraphToDot :: IO LazyByteString
testGraphToDot = runTestEff $ do
  serviceDefinition <- decodeUtf8 <$> FileSystem.readFile "test/fixtures/services/multiple-service-definitions.kdl"
  declarations <- assertRight "KDL file could not be parsed" $ KDL.decodeWith decodeServiceDocument serviceDefinition
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

testVariableParsing :: TestEff ()
testVariableParsing = do
  variableDefinition <- decodeUtf8 <$> FileSystem.readFile "test/fixtures/variables/variable-declaration.kdl"
  let expectedResult =
        Variable
          { varName = "env"
          , varType = "text"
          , varDescription = "Deployment environment in a specific cloud"
          , varValues = Just ["dev", "prod", "staging"]
          }
  result <- assertRight "KDL file could not be parsed" $ KDL.decodeWith (KDL.document variableDecoder) variableDefinition
  assertEqual
    "Expected service definition"
    expectedResult
    result

testVariableWithoutValidationParsing :: TestEff ()
testVariableWithoutValidationParsing = do
  variableDefinition <- decodeUtf8 <$> FileSystem.readFile "test/fixtures/variables/variable-without-validation.kdl"
  let expectedResult =
        Variable
          { varName = "no-validation"
          , varType = "text"
          , varDescription = "Variable without validation"
          , varValues = Nothing
          }
  result <- assertRight "KDL file could not be parsed" $ KDL.decodeWith (KDL.document variableDecoder) variableDefinition
  assertEqual
    "Expected service definition"
    expectedResult
    result

testVariableWithMissingValues :: TestEff ()
testVariableWithMissingValues = do
  variableDefinition <- decodeUtf8 <$> FileSystem.readFile "test/fixtures/variables/empty-values-variable.kdl"
  result <- assertLeft "KDL file should not be parsed" $ KDL.decodeWith (KDL.document variableDecoder) variableDefinition
  assertEqual
    "Expected service definition"
    "At: variable #0\n  Field \"values\" of variable \"no-validation\" cannot be empty"
    (KDL.renderDecodeError result)

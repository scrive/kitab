{-# LANGUAGE OverloadedLists #-}

module Test.ModelTests where

import Data.List.NonEmpty qualified as NE
import Data.Text.Encoding
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import KDL qualified
import Test.Tasty
import Validation (validationToEither)

import Core.Filtering
import Core.Filtering.Types
import Core.Graph
import Core.Model.FQDN
import Core.Model.Reference
import Core.Model.Service
import Core.Model.ServiceName
import Core.Validation
import Parser (decodeServiceDocument)
import Parser.Types
import Test.Utils

test :: TestTree
test =
  testGroup
    "Domain model"
    [ testGroup
        "Graph consistency validation"
        [ testThat "Parallel connections are forbidden" testParallelConnectionsDetection
        , testThat "Self-referential connections are forbidden" testSelfReferentialConnections
        , testThat "Mismatched connections are forbiddden" testMismatchedConnections
        ]
    , testGroup
        "Property filtering"
        [ testThat "Filtering FQDNs on the 'env' property" testFqdnEnvFiltering
        ]
    ]

testParallelConnectionsDetection :: TestEff ()
testParallelConnectionsDetection = do
  let graph = buildGraph [Service "A" defaultServiceInfo [Connection (ServiceName "B") HTTPS [], Connection (ServiceName "B") FunctionCall []] [] []] []
  validationError <- assertLeft "Graph is not validated" $ validationToEither (checkGraph graph)
  assertEqual
    "Parallel edges"
    (NE.singleton (Parallel (ServiceRef $ ServiceName "A") (ServiceRef $ ServiceName "B") [HTTPS, FunctionCall]))
    validationError

testSelfReferentialConnections :: TestEff ()
testSelfReferentialConnections = do
  let graph = buildGraph [Service "A" defaultServiceInfo [Connection (ServiceName "A") HTTPS []] [] []] []
  validationError <- assertLeft "Graph is not validated" $ validationToEither (checkGraph graph)
  assertEqual
    "Self-Referential edges"
    (NE.singleton (SelfReferential (ServiceRef $ ServiceName "A")))
    validationError

testMismatchedConnections :: TestEff ()
testMismatchedConnections = do
  let graph = buildGraph [Service "A" defaultServiceInfo [Connection (ServiceName "B") HTTPS []] [] [], Service "B" defaultServiceInfo [Connection (ServiceName "A") FunctionCall []] [] []] []
  validationError <- assertLeft "Graph is not validated" $ validationToEither (checkGraph graph)
  assertEqual
    "Mismatched connections"
    (NE.singleton (Mismatched ((ServiceRef (ServiceName "A"), ServiceRef (ServiceName "B"), HTTPS), (ServiceRef (ServiceName "B"), ServiceRef (ServiceName "A"), FunctionCall))))
    validationError

testFqdnEnvFiltering :: TestEff ()
testFqdnEnvFiltering = do
  serviceDefinition <- decodeUtf8 <$> FileSystem.readFile "test/fixtures/services/env-specific-values.kdl"
  declarations <-
    assertRight "KDL file could not be parsed" $
      KDL.decodeWith decodeServiceDocument serviceDefinition
  let services =
        mapMaybe
          ( \case
              ServiceDeclaration s
                | s.serviceName == "cache" -> Just s
                | otherwise -> Nothing
              _ -> Nothing
          )
          declarations
  let filterAction = Equals "env" "prod"
  let actual = concatMap ((\s -> s.serviceInfo.serviceFqdns) . filterServiceOnAttributes filterAction) services
  assertEqual
    "FQDNs filtered"
    [FQDN {fqdn = "prod.valkey.internal.network", props = [("env", "prod")]}]
    actual

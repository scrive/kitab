{-# LANGUAGE OverloadedLists #-}

module Test.ModelTests where

import Data.List.NonEmpty qualified as NE
import Test.Tasty
import Validation (validationToEither)

import Core.Graph
import Core.Model.Reference
import Core.Model.Service
import Core.Model.ServiceName
import Core.Validation
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
    ]

testParallelConnectionsDetection :: TestEff Unit
testParallelConnectionsDetection = do
  let service =
        emptyService
          { serviceName = "A"
          , serviceInfo = emptyServiceInfo
          , serviceConnections =
              [Connection (ServiceName "B") HTTPS [], Connection (ServiceName "B") FunctionCall []]
          }
  let graph = buildGraph [service] []
  validationError <- assertLeft "Graph is not validated" $ validationToEither (checkGraph graph)
  assertEqual
    "Parallel edges"
    (NE.singleton (Parallel (ServiceRef $ ServiceName "A") (ServiceRef $ ServiceName "B") [HTTPS, FunctionCall]))
    validationError

testSelfReferentialConnections :: TestEff Unit
testSelfReferentialConnections = do
  let service =
        emptyService
          { serviceName = "A"
          , serviceInfo = emptyServiceInfo
          , serviceConnections =
              [Connection (ServiceName "A") HTTPS []]
          }
  let graph = buildGraph [service] []
  validationError <- assertLeft "Graph is not validated" $ validationToEither (checkGraph graph)
  assertEqual
    "Self-Referential edges"
    (NE.singleton (SelfReferential (ServiceRef $ ServiceName "A")))
    validationError

testMismatchedConnections :: TestEff Unit
testMismatchedConnections = do
  let serviceA =
        emptyService
          { serviceName = "A"
          , serviceInfo = emptyServiceInfo
          , serviceConnections =
              [Connection (ServiceName "B") HTTPS []]
          }
      serviceB =
        emptyService
          { serviceName = "B"
          , serviceInfo = emptyServiceInfo
          , serviceConnections =
              [Connection (ServiceName "A") FunctionCall []]
          }
  let graph = buildGraph [serviceA, serviceB] []
  validationError <- assertLeft "Graph is not validated" $ validationToEither (checkGraph graph)
  assertEqual
    "Mismatched connections"
    (NE.singleton (Mismatched ((ServiceRef (ServiceName "A"), ServiceRef (ServiceName "B"), HTTPS), (ServiceRef (ServiceName "B"), ServiceRef (ServiceName "A"), FunctionCall))))
    validationError

{-# LANGUAGE OverloadedLists #-}

module Test.ModelTests where

import Data.List.NonEmpty qualified as NE
import Test.Tasty
import Validation (validationToEither)

import Core.Graph
import Core.Model.Service
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

testParallelConnectionsDetection :: TestEff ()
testParallelConnectionsDetection = do
  let graph = buildGraph [Service "A" defaultServiceInfo [Connection (ServiceReference "B" Nothing) HTTPS [], Connection (ServiceReference "B" Nothing) FunctionCall []] []]
  validationError <- assertLeft "Graph is not validated" $ validationToEither (checkGraph graph)
  assertEqual
    "Parallel edges"
    (NE.singleton (Parallel (ServiceReference "A" Nothing) (ServiceReference "B" Nothing) [HTTPS, FunctionCall]))
    validationError

testSelfReferentialConnections :: TestEff ()
testSelfReferentialConnections = do
  let graph = buildGraph [Service "A" defaultServiceInfo [Connection (ServiceReference "A" Nothing) HTTPS []] []]
  validationError <- assertLeft "Graph is not validated" $ validationToEither (checkGraph graph)
  assertEqual
    "Self-Referential edges"
    (NE.singleton (SelfReferential (ServiceReference "A" Nothing)))
    validationError

testMismatchedConnections :: TestEff ()
testMismatchedConnections = do
  let graph = buildGraph [Service "A" defaultServiceInfo [Connection (ServiceReference "B" Nothing) HTTPS []] [], Service "B" defaultServiceInfo [Connection (ServiceReference "A" Nothing) FunctionCall []] []]
  validationError <- assertLeft "Graph is not validated" $ validationToEither (checkGraph graph)
  assertEqual
    "Mismatched connections"
    (NE.singleton (Mismatched ((ServiceReference "A" Nothing, ServiceReference "B" Nothing, HTTPS), (ServiceReference "A" Nothing, ServiceReference "B" Nothing, FunctionCall))))
    validationError

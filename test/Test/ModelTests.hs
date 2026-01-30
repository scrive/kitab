module Test.ModelTests where

import Data.List.NonEmpty qualified as NE
import Test.Tasty
import Validation (validationToEither)

import Core.Model
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
  let graph = build [Service "A" defaultServiceInfo [Connection "B" HTTPS, Connection "B" FunctionCall]]
  validationError <- assertLeft "Graph is not validated" $ validationToEither (checkGraph graph)
  assertEqual
    "Parallel edges"
    (NE.singleton (Parallel "A" "B" [HTTPS, FunctionCall]))
    validationError

testSelfReferentialConnections :: TestEff ()
testSelfReferentialConnections = do
  let graph = build [Service "A" defaultServiceInfo [Connection "A" HTTPS]]
  validationError <- assertLeft "Graph is not validated" $ validationToEither (checkGraph graph)
  assertEqual
    "Self-Referential edges"
    (NE.singleton (SelfReferential "A"))
    validationError

testMismatchedConnections :: TestEff ()
testMismatchedConnections = do
  let graph = build [Service "A" defaultServiceInfo [Connection "B" HTTPS], Service "B" defaultServiceInfo [Connection "A" FunctionCall]]
  validationError <- assertLeft "Graph is not validated" $ validationToEither (checkGraph graph)
  assertEqual
    "Mismatched connections"
    (NE.singleton (Mismatched (("A", "B", HTTPS), ("B", "A", FunctionCall))))
    validationError

{-# LANGUAGE OverloadedLists #-}

module Test.ModelTests where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Test.Tasty
import Validation (validationToEither)

import Core.Graph
import Core.Model.ContextName
import Core.Model.Reference
import Core.Model.Service
import Core.Model.ServiceContext
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
    , testGroup
        "Context hierarchies"
        [ testThat "Nested contexts yield full paths" testNestedContextPaths
        , testThat "Deeply nested contexts accumulate the full path" testDeeplyNestedContextPaths
        , testThat "contextPaths keeps every declaration of a duplicated name" testDuplicateContextPaths
        , testThat "contextHierarchies keeps the last declaration of a duplicated name" testDuplicateContextHierarchies
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

testNestedContextPaths :: TestEff Unit
testNestedContextPaths = do
  let contexts = [ServiceContext {contextName = "company", subContexts = [ServiceContext {contextName = "k8s", subContexts = []}]}]
  assertEqual
    "A nested context carries its parent in the path"
    [("company", ["company"]), ("k8s", ["company", "k8s"])]
    (contextPaths contexts)

testDeeplyNestedContextPaths :: TestEff Unit
testDeeplyNestedContextPaths = do
  let contexts =
        [ ServiceContext
            { contextName = "a"
            , subContexts = [ServiceContext {contextName = "b", subContexts = [ServiceContext {contextName = "c", subContexts = []}]}]
            }
        ]
  assertEqual
    "Each level accumulates the full path from the root"
    [("a", ["a"]), ("b", ["a", "b"]), ("c", ["a", "b", "c"])]
    (contextPaths contexts)

testDuplicateContextPaths :: TestEff Unit
testDuplicateContextPaths = do
  let duplicateNameContexts =
        [ ServiceContext {contextName = "x", subContexts = [ServiceContext {contextName = "dup", subContexts = []}]}
        , ServiceContext {contextName = "y", subContexts = [ServiceContext {contextName = "dup", subContexts = []}]}
        ]
  assertEqual
    "contextPaths retains both declarations of the duplicated name"
    [("x", ["x"]), ("dup", ["x", "dup"]), ("y", ["y"]), ("dup", ["y", "dup"])]
    (contextPaths duplicateNameContexts)

testDuplicateContextHierarchies :: TestEff Unit
testDuplicateContextHierarchies = do
  let duplicateNameContexts =
        [ ServiceContext {contextName = "x", subContexts = [ServiceContext {contextName = "dup", subContexts = []}]}
        , ServiceContext {contextName = "y", subContexts = [ServiceContext {contextName = "dup", subContexts = []}]}
        ]
  let hierarchies = contextHierarchies duplicateNameContexts
  assertEqual
    "The last declaration of the duplicated name wins"
    (Just ["y", "dup"])
    (Map.lookup "dup" hierarchies)

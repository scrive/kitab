{-# LANGUAGE OverloadedLists #-}

module Test.ModelTests where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Test.Tasty
import Validation (validationToEither)

import Core.Graph
import Core.Model.Inventory
import Core.Model.InventoryVariable
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
    , testGroup
        "Inventory"
        [ testThat
            "Inventory merge"
            testInventoryMerge
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

testInventoryMerge :: TestEff ()
testInventoryMerge = do
  let baseOpenSearchFQDN =
        InventoryVariable
          { name = "opensearch-fqdn"
          , value = "opensearch.aws.internal.network"
          , description = Just "OpenSearch in use in our AWS environment"
          }
  let i1 =
        Inventory
          { name = "base"
          , vars =
              Map.fromList
                [ ("opensearch-fqdn", baseOpenSearchFQDN)
                ]
          }
  let overridenOpenSearchFQDN =
        InventoryVariable
          { name = "opensearch-fqdn"
          , value = "opensearch.aws.2423423f.internal.network"
          , description = Just "OpenSearch in use in our AWS environment for dev specifically"
          }
  let i2 =
        Inventory
          { name = "aws.dev"
          , vars = Map.fromList [("opensearch-fqdn", overridenOpenSearchFQDN)]
          }
  let aggregatedInventory =
        AggregatedInventory
          { names = ["base", "aws.dev"]
          , aggregatedVars = Map.fromList [("opensearch-fqdn", overridenOpenSearchFQDN)]
          }

  assertEqual
    "Inventories are merged correctly"
    (mergeInventories [i1, i2])
    aggregatedInventory

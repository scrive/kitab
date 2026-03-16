module Test.InventoryTests where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Test.Tasty

import Core.Model.Inventory
import Core.Model.InventoryVariable
import Driver.Inventory
import Test.Utils

test :: TestTree
test =
  testGroup
    "Inventory"
    [ testThat
        "Inventory merge"
        testInventoryMerge
    , testThat
        "Collecting inventories from filesystem"
        testCollectingInventoriesFromFileSystem
    ]

testInventoryMerge :: TestEff ()
testInventoryMerge = do
  let baseOpenSearchFQDN =
        InventoryVariable
          { name = "opensearch-fqdn"
          , value = "opensearch.aws.internal.network"
          , description = Just "OpenSearch in use in our AWS environment"
          }
  let baseRedisFQDN =
        InventoryVariable
          { name = "redis-fqdn"
          , value = "redis.aws.com"
          , description = Nothing
          }
  let i1 =
        Inventory
          { name = "base"
          , vars =
              Map.fromList
                [ ("opensearch-fqdn", baseOpenSearchFQDN)
                , ("redis-fqdn", baseRedisFQDN)
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
          , aggregatedVars = Map.fromList [("opensearch-fqdn", overridenOpenSearchFQDN), ("redis-fqdn", baseRedisFQDN)]
          }
  assertEqual
    "Inventories are merged correctly"
    (mergeInventories [i1, i2])
    aggregatedInventory

testCollectingInventoriesFromFileSystem :: TestEff ()
testCollectingInventoriesFromFileSystem = do
  inventories <- listInventoryFiles "./test/fixtures/inventory"
  assertEqual
    "Inventories"
    (Set.fromList ["./test/fixtures/inventory/aws/staging/inventory.kdl", "./test/fixtures/inventory/aws/prod/inventory.kdl", "./test/fixtures/inventory/aws/dev/inventory.kdl"])
    (Set.fromList inventories)

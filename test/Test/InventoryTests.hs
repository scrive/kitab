module Test.InventoryTests where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC.Stack
import KDL qualified
import Test.Tasty

import Core.Model.Inventory
import Core.Model.InventoryVariable
import Core.Model.Service
import Driver.Inventory
import Driver.Variable
import Parser.Service
import Test.Utils
import Core.Variable

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
    , testThat "Variables are resolved via inventory" testResolvingVariableFromInventory
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

testResolvingVariableFromInventory :: TestEff ()
testResolvingVariableFromInventory = do
  let opensearchFqdn =
        InventoryVariable
          { name = "opensearch-fqdn"
          , value = "opensearch.aws.2423423f.internal.network"
          , description = Just "OpenSearch in use in our AWS environment for dev specifically"
          }
  let aggregatedInventory =
        AggregatedInventory
          { names = ["base", "aws.dev"]
          , aggregatedVars = Map.fromList [("opensearch-fqdn", opensearchFqdn)]
          }

  service <- assertParseFile (KDL.document serviceDecoder) "test/fixtures/service-with-var.kdl"
  assertEqual 
    "OpenSearch FQDN is not yet resolved"
    (Just (Left (Var "opensearch-fqdn")))
    service.serviceInfo.serviceFqdn

  resolvedService <- resolveServiceVars aggregatedInventory service
  assertEqual
    "OpenSearch FQDN is resolved"
    (Just (Right "opensearch.aws.2423423f.internal.network"))
    resolvedService.serviceInfo.serviceFqdn

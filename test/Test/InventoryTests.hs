{-# LANGUAGE QuasiQuotes #-}

module Test.InventoryTests where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC.Stack
import KDL qualified
import System.OsPath
import System.OsPath qualified as OsPath
import Test.Tasty

import Core.Model.Inventory
import Core.Model.Inventory.Aggregated
import Core.Model.Inventory.Selector
import Core.Model.InventoryVariable
import Core.Model.Service
import Core.Variable
import Driver.Inventory
import Driver.Variable
import Parser.V1.Service
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
    , testThat "Variables are resolved via inventory" testResolvingVariableFromInventory
    , testThat
        "Inventories detected from filesystem are merged"
        testInventoryMergeFromFileSystem
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
          { attributes = Map.fromList [("cloud", "aws")]
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
          { attributes = Map.fromList [("cloud", "aws"), ("env", "dev")]
          , vars = Map.fromList [("opensearch-fqdn", overridenOpenSearchFQDN)]
          }
  let expectedAggregatedInventory =
        AggregatedInventory
          { aggregatedAttributes = Map.fromList [("cloud", "aws"), ("env", "dev")]
          , aggregatedVars = Map.fromList [("opensearch-fqdn", overridenOpenSearchFQDN), ("redis-fqdn", baseRedisFQDN)]
          }
  assertEqual
    "Inventories are merged correctly"
    expectedAggregatedInventory
    (mergeInventories (Selector (Just "aws")) (Selector Nothing) (Selector (Just "dev")) [i1, i2])

testCollectingInventoriesFromFileSystem :: TestEff ()
testCollectingInventoriesFromFileSystem = do
  inventories <-
    listInventoryFiles [osp|./test/fixtures/inventory|]
      >>= traverse OsPath.decodeUtf
  assertEqual
    "Inventories"
    (Set.fromList ["./test/fixtures/inventory/aws/staging/inventory.kdl", "./test/fixtures/inventory/aws/prod/inventory.kdl", "./test/fixtures/inventory/aws/dev/inventory.kdl", "./test/fixtures/inventory/aws/inventory.kdl"])
    (Set.fromList inventories)

testResolvingVariableFromInventory :: HasCallStack => TestEff ()
testResolvingVariableFromInventory = do
  let opensearchFqdn =
        InventoryVariable
          { name = "opensearch-fqdn"
          , value = "opensearch.aws.2423423f.internal.network"
          , description = Just "OpenSearch in use in our AWS environment for dev specifically"
          }
  let aggregatedInventory =
        AggregatedInventory
          { aggregatedAttributes = Map.fromList [("cloud", "aws"), ("env", "dev")]
          , aggregatedVars = Map.fromList [("opensearch-fqdn", opensearchFqdn)]
          }

  service <- assertParse (KDL.document serviceDecoder) "test/fixtures/service-with-var.kdl"
  assertEqual
    "OpenSearch FQDN is not yet resolved"
    (Just (Left (Var "opensearch-fqdn")))
    service.serviceInfo.serviceFqdn

  resolvedService <- resolveServiceVars aggregatedInventory service
  assertEqual
    "OpenSearch FQDN is resolved"
    (Just (Right "opensearch.aws.2423423f.internal.network"))
    resolvedService.serviceInfo.serviceFqdn

testInventoryMergeFromFileSystem :: TestEff ()
testInventoryMergeFromFileSystem = do
  inventories <- getInventories [osp|./test/fixtures/inventory|]
  let aggregatedInventory = mergeInventories (Selector (Just "aws")) (Selector Nothing) (Selector (Just "dev")) inventories

  assertEqual
    "Inventories are aggregated correctly"
    AggregatedInventory {aggregatedAttributes = Map.fromList [("cloud", "aws"), ("env", "dev")], aggregatedVars = Map.fromList [("my-var", InventoryVariable {name = "my-var", value = "aws-base-inventory", description = Just "Base value for the AWS environment"})]}
    aggregatedInventory

  let service = emptyService {serviceName = "app", serviceInfo = ServiceInfo {serviceFqdn = Just (Left (Var "my-var")), serviceContext = Nothing, servicePorts = Set.fromList []}}

  resolvedService <- resolveServiceVars aggregatedInventory service

  assertEqual
    "Variable resolved through filesystem inventories"
    (Just (Right "aws-dev-value"))
    resolvedService.serviceInfo.serviceFqdn

module Core.Model.Inventory.Aggregated
  ( AggregatedInventory (..)
  , mergeInventories
  , lookup
  ) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Prelude hiding (lookup)

import Core.Model.Inventory
import Core.Model.Inventory.Selector
import Core.Model.InventoryVariable

data AggregatedInventory = AggregatedInventory
  { aggregatedAttributes :: Map Text Text
  , aggregatedVars :: Map VariableName InventoryVariable
  }
  deriving stock (Eq, Ord, Show)

-- This uses 'Map.union' which is left-biased, which means that in case
-- of duplicates, the currently-processed element will be preferred
-- to the accumulator.
mergeInventories
  :: Selector Cloud
  -> Selector Region
  -> Selector Environment
  -> List Inventory
  -> AggregatedInventory
mergeInventories cloudSelector regionSelector environmentSelector inventories =
  let selectorQuery = buildSelectedAttributesMap cloudSelector regionSelector environmentSelector
      aggregatedVars =
        inventories
          & List.filter (\inventory -> inventory.attributes `Map.isSubmapOf` selectorQuery)
          & List.sortOn (Down . Map.size . (.attributes))
          & List.foldl' (\acc inventory -> Map.union acc inventory.vars) Map.empty
  in AggregatedInventory
       { aggregatedAttributes = selectorQuery
       , aggregatedVars
       }
buildSelectedAttributesMap
  :: Selector Cloud
  -> Selector Region
  -> Selector Environment
  -> Map Text Text
buildSelectedAttributesMap cloudSelector regionSelector environmentSelector =
  let cloudArg = case cloudSelector.value of
        Nothing -> []
        Just cloud -> [("cloud", cloud)]
      regionArg = case regionSelector.value of
        Nothing -> []
        Just region -> [("region", region)]
      envArg = case environmentSelector.value of
        Nothing -> []
        Just env -> [("env", env)]
      args = cloudArg <> regionArg <> envArg
  in Map.fromList args

lookup
  :: AggregatedInventory
  -> VariableName
  -> Maybe InventoryVariable
lookup AggregatedInventory {aggregatedVars} var = Map.lookup var aggregatedVars

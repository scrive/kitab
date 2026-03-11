module Core.Model.Inventory where

import Data.List qualified as List
import Data.Map.Strict qualified as Map

import Core.Model.InventoryVariable

data Inventory = Inventory
  { name :: Text
  , vars :: Map VariableName InventoryVariable
  }
  deriving stock (Eq, Ord, Show)

data AggregatedInventory = AggregatedInventory
  { names :: List Text
  , aggregatedVars :: Map VariableName InventoryVariable
  }
  deriving stock (Eq, Ord, Show)

-- This uses 'Map.union' which is left-biased, which means that in case
-- of duplicates, the currently-processed element will be preferred
-- to the accumulator.
mergeInventories :: List Inventory -> AggregatedInventory
mergeInventories inventories =
  let (aggregatedNames, aggregatedVars) =
        List.foldl'
          (\(accNames, accVars) inventory -> (inventory.name : accNames, Map.union inventory.vars accVars))
          (mempty, Map.empty)
          inventories
      names = List.reverse aggregatedNames
  in AggregatedInventory {names, aggregatedVars}

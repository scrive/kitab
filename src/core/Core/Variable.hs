module Core.Variable where

import Data.Map.Strict qualified as Map

import Core.Model.Inventory
import Core.Model.InventoryVariable

newtype Var = Var VariableName
  deriving stock (Eq, Ord, Show)

lookup
  :: AggregatedInventory
  -> VariableName
  -> Maybe Text
lookup AggregatedInventory {aggregatedVars} var = do
  aggregatedVars
    & Map.lookup var
    & fmap (.value)

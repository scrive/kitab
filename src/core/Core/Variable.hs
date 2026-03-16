module Core.Variable where

import Data.Map.Strict qualified as Map
import Effectful

import Core.Model.Inventory
import Core.Model.InventoryVariable

data Var = Var VariableName
  deriving stock (Eq, Ord, Show)

lookup
  :: AggregatedInventory
  -> Var
  -> Eff es Text
lookup AggregatedInventory {aggregatedVars} (Var var) = do
  case Map.lookup var aggregatedVars of
    Nothing -> pure ""
    Just inventoryVar -> pure inventoryVar.value

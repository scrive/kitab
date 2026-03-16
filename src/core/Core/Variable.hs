module Core.Variable where

import Data.Map.Strict qualified as Map
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader

import Core.Model.Inventory
import Core.Model.InventoryVariable

data Var = Var VariableName
  deriving stock (Eq, Ord, Show)

lookup
  :: Reader AggregatedInventory :> es
  => Var
  -> Eff es Text
lookup (Var var) = do
  AggregatedInventory {aggregatedVars} <- Reader.ask
  case Map.lookup var aggregatedVars of
    Nothing -> pure ""
    Just inventoryVar -> pure inventoryVar.value

module Core.Model.Inventory where

import Data.Map.Strict qualified as Map

import Core.Model.InventoryVariable

data Inventory = Inventory
  { attributes :: Map Text Text
  , vars :: Map VariableName InventoryVariable
  }
  deriving stock (Eq, Ord, Show)

emptyInventory :: Inventory
emptyInventory =
  Inventory
    { attributes = Map.empty
    , vars = Map.empty
    }

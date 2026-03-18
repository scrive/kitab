module Core.Model.Inventory where

import Core.Model.InventoryVariable

data Inventory = Inventory
  { attributes :: Map Text Text
  , vars :: Map VariableName InventoryVariable
  }
  deriving stock (Eq, Ord, Show)

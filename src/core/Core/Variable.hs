module Core.Variable where

import Core.Model.InventoryVariable

newtype Var = Var VariableName
  deriving stock (Eq, Ord, Show)

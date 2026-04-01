module Core.Model.InventoryVariable where

import Data.String

newtype VariableName = VariableName Text
  deriving newtype (Eq, Ord, Show, Display, IsString)

data InventoryVariable = InventoryVariable
  { name :: VariableName
  , value :: Text
  , description :: Maybe Text
  }
  deriving stock (Show)

instance Eq InventoryVariable where
  v1 == v2 = v1.name == v2.name
instance Ord InventoryVariable where
  compare v1 v2 = compare v1.name v2.name

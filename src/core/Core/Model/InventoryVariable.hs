module Core.Model.InventoryVariable where

import Control.DeepSeq
import Data.String
import Prettyprinter

newtype VariableName = VariableName Text
  deriving newtype (Eq, Ord, Show, IsString, Pretty, Display, NFData)

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

emptyInventoryVariable :: InventoryVariable
emptyInventoryVariable =
  InventoryVariable
    { name = ""
    , value = ""
    , description = Nothing
    }

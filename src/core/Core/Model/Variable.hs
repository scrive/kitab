module Core.Model.Variable where

import Data.Set (Set)

data Variable = Variable
  { varName :: Text
  , varType :: Text
  , varDescription :: Text
  , varValues :: Maybe (Set Text)
  }
  deriving stock (Eq, Ord, Show)

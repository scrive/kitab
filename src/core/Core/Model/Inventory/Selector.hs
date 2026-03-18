{-# LANGUAGE TypeData #-}

module Core.Model.Inventory.Selector where

type data SelectorLabel
  = Cloud
  | Region
  | Environment

-- | A selector allows the end-user to specify the value
-- of an inventory attribute. If none are provided, then
-- it is set to 'Nothing'.
data Selector (label :: SelectorLabel) = Selector
  { value :: Maybe Text
  }
  deriving stock (Eq, Ord, Show)

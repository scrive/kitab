module Core.Filtering.Types where

import Data.Map.Strict qualified as Map

data FilterAction
  = Equals
      Text
      -- ^ Key
      Text
      -- ^ Value
  deriving stock (Eq, Ord, Show)

interpretFilter :: FilterAction -> Map Text Text -> Bool
interpretFilter action props = case action of
  Equals key value ->
    let mProp = Map.lookup key props
    in Just value == mProp

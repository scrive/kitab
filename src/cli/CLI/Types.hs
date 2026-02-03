module CLI.Types where

import System.OsPath

data Options = Options
  { quiet :: Bool
  , format :: Text
  , inputs :: List OsPath
  }
  deriving stock (Eq, Ord, Show)

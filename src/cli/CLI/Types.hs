module CLI.Types where

import System.OsPath

data Options = Options
  { quiet :: Bool
  , format :: OutputFormat
  , inputs :: List OsPath
  , outputDir :: OsPath
  }
  deriving stock (Eq, Ord, Show)

data OutputFormat
  = PumlFormat
  | CiliumFormat
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Display OutputFormat where
  displayBuilder PumlFormat = "puml"
  displayBuilder CiliumFormat = "cilium"

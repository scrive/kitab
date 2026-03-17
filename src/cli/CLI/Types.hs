module CLI.Types where

import System.OsPath

import Core.Model.ContextName

data Options = Options
  { quiet :: Bool
  , format :: OutputFormat
  , outputDir :: OsPath
  , contextFilters :: List ContextName
  , inventory :: Maybe OsPath
  , inputs :: List OsPath
  }
  deriving stock (Eq, Ord, Show)

data OutputFormat
  = PumlFormat
  | CiliumFormat
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Display OutputFormat where
  displayBuilder PumlFormat = "puml"
  displayBuilder CiliumFormat = "cilium"

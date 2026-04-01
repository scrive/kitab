module CLI.Types where

import CLI.Cmd.Generate

data Command
  = CmdGenerate GenerateOptions
  deriving stock (Eq, Ord, Show)

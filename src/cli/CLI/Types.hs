module CLI.Types where

import CLI.Cmd.Dump
import CLI.Cmd.Generate

data Command
  = CmdGenerate GenerateOptions
  | CmdDump DumpOptions
  deriving stock (Eq, Ord, Show)

module Main (main) where

import Control.Exception.Backtrace
import Data.List.NonEmpty
import Data.Text.IO qualified as T
import Effectful
import Effectful.Console.ByteString (Console, runConsole)
import Effectful.Environment (Environment, runEnvironment)
import Effectful.Error.Static
import Effectful.FileSystem
import Options.Applicative
import System.Exit qualified as System
import System.IO

import CLI
import CLI.Cmd.Dump (runDump)
import CLI.Cmd.Generate (runGenerate)
import CLI.Error
import CLI.Types

main :: IO Unit
main = do
  hSetBuffering stdout LineBuffering
  setBacktraceMechanismState IPEBacktrace True
  parseResult <- execParser parserOptions
  result <-
    runOptions parseResult
      & runFileSystem
      & runErrorNoCallStack @_
      & runConsole
      & runEnvironment
      & runEff
  case result of
    Right _ -> pure ()
    Left errors -> do
      traverse_ @NonEmpty (T.putStrLn . display @CLIError) errors
      System.exitFailure

runOptions
  :: (Console :> es, FileSystem :> es, Error (NonEmpty CLIError) :> es, Environment :> es)
  => Command
  -> Eff es Unit
runOptions (CmdGenerate cmdOptions) = runGenerate cmdOptions
runOptions (CmdDump cmdOptions) = runDump cmdOptions

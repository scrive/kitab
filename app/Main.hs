module Main (main) where

import Data.List.NonEmpty
import Data.Text.IO qualified as T
import Effectful
import Effectful.Error.Static
import Effectful.FileSystem
import Options.Applicative
import System.Exit qualified as System
import System.IO

import CLI
import CLI.Error
import Driver

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  parseResult <- execParser parserOptions
  result <-
    runOptions parseResult
      & runFileSystem
      & runErrorNoCallStack @_
      & runEff
  case result of
    Right _ -> pure ()
    Left errors -> do
      traverse_ @NonEmpty (T.putStrLn . display @CLIError) errors
      System.exitFailure

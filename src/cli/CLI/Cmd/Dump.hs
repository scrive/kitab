module CLI.Cmd.Dump
  ( DumpOptions (..)
  , runDump
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString qualified as Console
import Effectful.Error.Static (Error)
import Effectful.FileSystem (FileSystem)
import System.OsPath (OsPath)
import Text.Pretty.Simple (pShowNoColor)

import CLI.Error
import Driver.Parsing (parseFile, requireFile)

newtype DumpOptions = DumpOptions
  { input :: OsPath
  }
  deriving stock (Eq, Ord, Show)

runDump
  :: (Console :> es, FileSystem :> es, Error (NonEmpty CLIError) :> es)
  => DumpOptions
  -> Eff es Unit
runDump options = do
  requireFile options.input
  declarations <- parseFile options.input
  Console.putStrLn (T.encodeUtf8 (TL.toStrict (pShowNoColor declarations)))

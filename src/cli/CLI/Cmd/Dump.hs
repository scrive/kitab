module CLI.Cmd.Dump
  ( DumpOptions (..)
  , runDump
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString qualified as Console
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem
import Effectful.FileSystem qualified as FileSystem
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath
import Text.Pretty.Simple (pShowNoColor)

import CLI.Error
import Parser (parseKitabDocument)

newtype DumpOptions = DumpOptions
  { input :: OsPath
  }
  deriving stock (Eq, Ord, Show)

runDump
  :: (Console :> es, FileSystem :> es, Error (NonEmpty CLIError) :> es)
  => DumpOptions
  -> Eff es Unit
runDump options = do
  filePath <- OsPath.decodeUtf options.input
  fileDoesExist <- FileSystem.doesFileExist filePath
  if not fileDoesExist
    then Error.throwError . NE.singleton $ noFileAtProvidedLocation options.input
    else do
      fileContent <- FileSystem.readFile filePath
      case parseKitabDocument filePath (T.decodeUtf8 fileContent) of
        Left err -> Error.throwError . NE.singleton $ kdlParseError options.input err
        Right declarations ->
          Console.putStrLn (T.encodeUtf8 (TL.toStrict (pShowNoColor declarations)))

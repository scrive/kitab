module Driver.Parsing
  ( parseFile
  , parseInputs
  , requireFile
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text.Encoding qualified as T
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem (FileSystem, doesFileExist)
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

import CLI.Error
import Core.Variable (Var)
import Parser (parseKitabDocument)
import Parser.V1.Types (Declaration)

parseInputs
  :: (FileSystem :> es, Error (NonEmpty CLIError) :> es)
  => List OsPath
  -> Eff es (List (Declaration Var))
parseInputs inputs = do
  missing <- filterM (fmap not . fileExists) inputs
  case NE.nonEmpty missing of
    Just errors -> Error.throwError (fmap noFileAtProvidedLocation errors)
    Nothing -> concatForM inputs parseFile

-- | Throw a 'CLIError' unless a file exists at the given path.
requireFile
  :: (FileSystem :> es, Error (NonEmpty CLIError) :> es)
  => OsPath
  -> Eff es Unit
requireFile inputPath = do
  exists <- fileExists inputPath
  unless exists . Error.throwError . NE.singleton $ noFileAtProvidedLocation inputPath

fileExists :: FileSystem :> es => OsPath -> Eff es Bool
fileExists inputPath = OsPath.decodeUtf inputPath >>= doesFileExist

parseFile
  :: (FileSystem :> es, Error (NonEmpty CLIError) :> es)
  => OsPath
  -> Eff es (List (Declaration Var))
parseFile inputPath = do
  filePath <- OsPath.decodeUtf inputPath
  fileContent <- FileSystem.readFile filePath
  case parseKitabDocument filePath (T.decodeUtf8 fileContent) of
    Right declarations -> pure declarations
    Left err -> Error.throwError . NE.singleton $ kdlParseError inputPath err

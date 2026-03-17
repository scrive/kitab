-- |  This is where the logic for gathering inventories
module Driver.Inventory where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text.Encoding qualified as T
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import KDL qualified
import System.FilePath (takeFileName, (</>))
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

import CLI.Error (CLIError, kdlParseError)
import Core.Model.Inventory
import Parser.Inventory

getInventories
  :: (FileSystem :> es, Error (NonEmpty CLIError) :> es)
  => OsPath
  -> Eff es (List Inventory)
getInventories baseDir = do
  files <- listInventoryFiles baseDir
  parseInventories files

listInventoryFiles
  :: FileSystem :> es
  => OsPath
  -> Eff es (List OsPath)
listInventoryFiles baseDir =
  OsPath.decodeUtf baseDir
    >>= go []
    >>= traverse OsPath.encodeUtf
  where
    go state dirPath = do
      names <- FileSystem.listDirectory dirPath
      let paths = fmap (dirPath </>) names
      (dirPaths, filePaths) <- partitionM FileSystem.doesDirectoryExist paths
      let inventories = filter isValidInventory filePaths
      foldM go (inventories <> state) dirPaths
    isValidInventory name = takeFileName name == "inventory.kdl"

parseInventories
  :: (Error (NonEmpty CLIError) :> es, FileSystem :> es)
  => List OsPath
  -> Eff es (List Inventory)
parseInventories inventoryFilePaths = do
  concatForM inventoryFilePaths $ \inputPath -> do
    fileContent <- FileSystem.readFile =<< OsPath.decodeUtf inputPath
    let result = KDL.decodeWith (KDL.document inventoryDecoder) (T.decodeUtf8 fileContent)
    case result of
      Right a -> pure [a]
      Left err -> do
        Error.throwError . NE.singleton $ kdlParseError inputPath err

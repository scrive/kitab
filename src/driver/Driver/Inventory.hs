-- |  This is where the logic for gathering inventories
module Driver.Inventory
  ( getInventories
  , listInventoryFiles
  , parseInventories
  , printInventory
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString qualified as Console
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Effectful.FileSystem.IO.ByteString qualified as FileSystem
import KDL qualified
import Layoutz
import System.FilePath (takeExtension, (</>))
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

import CLI.Error (CLIError, kdlParseError)
import Core.Model.Inventory
import Core.Model.Inventory.Aggregated (AggregatedInventory (..))
import Core.Model.InventoryVariable
import Driver.Colours
import Parser.V1.Inventory

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
    isValidInventory name = takeExtension name == ".kdl"

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

-- | Render the aggregated inventory (selected attributes and resolved
-- variables) to the console as a bordered table.
printInventory :: Console :> es => TerminalColoursSettings -> AggregatedInventory -> Eff es Unit
printInventory coloursSettings AggregatedInventory {aggregatedAttributes, aggregatedVars} = do
  let tableElements =
        aggregatedVars
          & Map.elems
          <&> (\InventoryVariable {name, value} -> (T.unpack (display name), T.unpack value))
  let getAttribute attr = T.unpack $ Map.findWithDefault "N/A" attr aggregatedAttributes
  let inventoryTable =
        layout
          [ text "╭─ Inventory ───────────────────────────────────────"
          , margin
              "│"
              [ stylise coloursSettings StyleBold $ text "Attributes:"
              , margin
                  "  "
                  [kv [("Cloud", getAttribute "cloud"), ("Region", getAttribute "region"), ("Environment", getAttribute "env")]]
              ]
          , margin
              "│"
              [ stylise coloursSettings StyleBold $ text "Variables:"
              , margin
                  "  "
                  [kv tableElements]
              ]
          , text "╰───────────────────────────────────────────────────"
          ]

  Console.putStrLn (T.encodeUtf8 (renderText inventoryTable))

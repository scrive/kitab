module Driver.Output where

import Data.ByteString.Char8 qualified as BS8
import Data.Text.Encoding qualified as T
import Effectful
import Effectful.Console.ByteString
import Effectful.Console.ByteString qualified as Console
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO.ByteString qualified as FileSystem

import Driver.Verbosity

writeArtifact
  :: (FileSystem :> es, Console :> es)
  => VerbositySetting
  -> FilePath
  -> Text
  -> Eff es Unit
writeArtifact verbosity outputPath rendered = do
  when (isVerbose verbosity) (Console.putStrLn $ "Writing file " <> BS8.pack outputPath)
  FileSystem.writeFile outputPath (T.encodeUtf8 rendered)

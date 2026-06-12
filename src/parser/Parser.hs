module Parser (parseKitabDocument) where

import Data.Text qualified as T
import KDL

import Core.Variable
import Parser.V1 qualified as V1
import Parser.V1.Types

parseKitabDocument
  :: FilePath
  -> Text
  -> Either DecodeError (List (Declaration Var))
parseKitabDocument filepath content =
  case KDL.parseWith ParseConfig {filepath, includeSpans = True} content of
    Left parseError -> runDecodeM . decodeThrow $ DecodeError_ParseError parseError
    Right parsedDocument -> do
      decoder <- selectDecoder (KDL.getArg =<< KDL.lookupNode "version" parsedDocument)
      KDL.decodeDocWith decoder parsedDocument

selectDecoder :: Maybe Value -> Either DecodeError (KDL.DocumentDecoder (List (Declaration Var)))
selectDecoder = \case
  Nothing -> Right V1.decodeServiceDocument
  Just v -> case v.data_ of
    Number 1 -> Right V1.decodeServiceDocument
    Number i -> runDecodeM . decodeThrow $ DecodeError_Custom ("This kitab can only parse configuration format version 1. Found " <> T.pack (show i))
    _ -> runDecodeM . decodeThrow $ DecodeError_ValueDecodeFail "number" v

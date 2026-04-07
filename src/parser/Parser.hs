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
  let parsingResult = KDL.parseWith ParseConfig {filepath, includeSpans = True} content
  in case parsingResult of
       Left parseError -> runDecodeM . decodeThrow $ DecodeError_ParseError parseError
       Right parsedDocument ->
         case KDL.getArg =<< KDL.lookupNode "version" parsedDocument of
           Nothing -> KDL.decodeWith V1.decodeServiceDocument content
           Just v -> case v.data_ of
             Number 1 -> KDL.decodeWith V1.decodeServiceDocument content
             Number i -> runDecodeM . decodeThrow $ DecodeError_Custom ("This kitab can only parse configuration format version 1. Found " <> T.pack (show i))
             _ -> runDecodeM . decodeThrow $ DecodeError_ValueDecodeFail "number" v

module CLI.Filter.Parser
  ( cmpParser
  ) where

import Data.Text qualified as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Core.Filtering.Types

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

stringVal :: Parser Text
stringVal = stringLiteral <|> unquotedString
  where
    stringLiteral = lexeme $ T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))
    unquotedString = lexeme $ takeWhile1P (Just "unquoted string character") (\c -> c /= ' ' && c /= '=')

cmpParser :: Parser FilterAction
cmpParser = between sc eof $ do
  left <- stringVal
  void $ symbol "=="
  Equals left <$> stringVal

{-# LANGUAGE TemplateHaskell #-}

module CLI where

import Data.Version
import Development.GitRev
import Options.Applicative
import Options.Applicative.Help.Pretty
import System.OsPath
import System.OsPath qualified as OsPath

import CLI.Types
import Paths_kitab (version)

parseOptions :: Parser Options
parseOptions =
  Options
    <$> switch (long "quiet" <> help "Make the program less verbose")
    <*> option str (long "format" <> metavar "FORMAT" <> help "Output format")
    <*> many (option pathParser (long "input" <> metavar "FILE" <> help "input file"))

pathParser :: ReadM OsPath
pathParser = maybeReader OsPath.encodeUtf

parserOptions :: ParserInfo Options
parserOptions =
  info (parseOptions <**> simpleVersioner programVersion <**> helper) $
    header "Kitab — Infrastructure Documentation"
      <> progDescDoc (Just programDescription)
      <> footerDoc (Just programFooter)

programVersion :: String
programVersion =
  "kitab " <> showVersion version <> " commit " <> $(gitHash) <> ")"

programDescription :: Doc
programDescription =
  "Kitab aggregates service definition files and produces infrastructure configuration or documentation out of them"

programFooter :: Doc
programFooter = "Git repository: https://github.com/scrive/kitab"

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc =
  info
    ( simpleVersioner (showVersion version)
        <*> helper
        <*> opts
    )
    $ progDesc desc

{-# LANGUAGE TemplateHaskell #-}

module CLI where

import Data.List qualified as List
import Data.Text qualified as T
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
  let supportedFormats = T.unpack . display <$> ([minBound .. maxBound] :: [OutputFormat])
  in Options
       <$> switch (long "quiet" <> short 'q' <> help "Make the program less verbose")
       <*> option outputFormat (long "format" <> short 'f' <> metavar "FORMAT" <> help "Output format" <> completeWith supportedFormats)
       <*> some (option pathParser (long "input" <> short 'i' <> metavar "FILE" <> help "input file, can be specified multiple times" <> action "file"))
       <*> option pathParser (long "output" <> short 'o' <> metavar "FILE" <> help "Output file" <> action "file")

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
  "Kitab aggregates service definition files and produces infrastructure configuration or documentation."

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

outputFormat :: ReadM OutputFormat
outputFormat = eitherReader $
  \case
    "puml" -> Right PumlFormat
    "cilium" -> Right CiliumFormat
    _ ->
      let supportedFormats = T.unpack . display <$> ([minBound .. maxBound] :: [OutputFormat])
      in Left $ "Kitab only supports the following formats: " <> mconcat (List.intersperse ", " supportedFormats)

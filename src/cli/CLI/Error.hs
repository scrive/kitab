{-# LANGUAGE RecordWildCards #-}

module CLI.Error where

import Data.Text qualified as T
import Data.Word
import KDL (DecodeError)
import System.OsPath

import Core.Validation

data CLIErrorType
  = ParseError
  | FileDoesNotExist
  | GraphValidationError
  deriving stock (Eq, Show, Ord)

newtype ErrorCode = ErrorCode Word8
  deriving newtype (Eq, Show, Ord)

instance Display ErrorCode where
  displayBuilder (ErrorCode c) = "[KITAB-]" <> displayBuilder c <> "]"

data CLIError = CLIError
  { errorType :: CLIErrorType
  , errorCode :: ErrorCode
  , errorMessage :: Text
  }
  deriving stock (Eq, Ord, Show)

instance Display CLIError where
  displayBuilder CLIError {..} =
    displayBuilder errorCode
      <> " "
      <> displayBuilder errorMessage

noFileAtProvidedLocation :: OsPath -> CLIError
noFileAtProvidedLocation path =
  CLIError
    { errorType = FileDoesNotExist
    , errorCode = ErrorCode 100
    , errorMessage = "Could not find file at " <> T.show path <> "."
    }

kdlParseError :: OsPath -> DecodeError -> CLIError
kdlParseError path decodeError =
  CLIError
    { errorType = ParseError
    , errorCode = ErrorCode 234
    , errorMessage = "Could not parse file " <> T.show path <> ". Got the following error: " <> T.show decodeError
    }

graphValidationError :: ValidationError -> CLIError
graphValidationError validationError =
  CLIError
    { errorType = GraphValidationError
    , errorCode = ErrorCode 241
    , errorMessage = T.show validationError
    }

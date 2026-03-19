{-# LANGUAGE RecordWildCards #-}

module CLI.Error where

import Data.Text qualified as T
import KDL (DecodeError, renderDecodeError)
import System.OsPath

import Core.Model.ContextName
import Core.Model.InventoryVariable
import Core.Validation

data CLIErrorType
  = ParseError
  | FileDoesNotExist
  | GraphValidationError
  | UnkownContextFilter
  | VariableNotFound
  deriving stock (Eq, Show, Ord, Enum, Bounded)

instance Display CLIErrorType where
  displayBuilder = \case
    ParseError -> "Parse error"
    FileDoesNotExist -> "File does not exist"
    GraphValidationError -> "Graph validation error"
    UnkownContextFilter -> "Unknown context filter"
    VariableNotFound -> "Variable not found"

errorCodeFromType :: CLIErrorType -> ErrorCode
errorCodeFromType = \case
  ParseError -> ErrorCode 234
  FileDoesNotExist -> ErrorCode 100
  GraphValidationError -> ErrorCode 241
  UnkownContextFilter -> ErrorCode 154
  VariableNotFound -> ErrorCode 523

newtype ErrorCode = ErrorCode Word
  deriving newtype (Eq, Show, Ord)

instance Display ErrorCode where
  displayBuilder (ErrorCode c) = "[KITAB-" <> displayBuilder c <> "]"

data CLIError = CLIError
  { errorType :: CLIErrorType
  , errorCode :: ErrorCode
  , errorMessage :: Text
  }
  deriving stock (Eq, Ord, Show)

instance Display CLIError where
  displayBuilder CLIError {..} =
    "Error: " <> displayBuilder errorCode <> " " <> displayBuilder errorMessage

noFileAtProvidedLocation :: OsPath -> CLIError
noFileAtProvidedLocation path =
  CLIError
    { errorType = FileDoesNotExist
    , errorCode = errorCodeFromType FileDoesNotExist
    , errorMessage = "Could not find file at " <> T.show path <> "."
    }

kdlParseError :: OsPath -> DecodeError -> CLIError
kdlParseError path decodeError =
  CLIError
    { errorType = ParseError
    , errorCode = errorCodeFromType ParseError
    , errorMessage = "Could not parse file " <> T.show path <> ". Got the following error: " <> renderDecodeError decodeError
    }

graphValidationError :: ValidationError -> CLIError
graphValidationError validationError =
  CLIError
    { errorType = GraphValidationError
    , errorCode = errorCodeFromType GraphValidationError
    , errorMessage = T.show validationError
    }

unknownContextFilter :: ContextName -> CLIError
unknownContextFilter contextFilter =
  CLIError
    { errorType = UnkownContextFilter
    , errorCode = errorCodeFromType UnkownContextFilter
    , errorMessage = "Could not find context " <> display contextFilter <> " to filter services."
    }

variableNotFound :: VariableName -> CLIError
variableNotFound expectedVariable =
  CLIError
    { errorType = VariableNotFound
    , errorCode = errorCodeFromType VariableNotFound
    , errorMessage = "Could not find a definition for " <> display expectedVariable <> " in provided inventories."
    }

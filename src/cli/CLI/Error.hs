{-# LANGUAGE RecordWildCards #-}

module CLI.Error where

import Data.Text qualified as T
import KDL (DecodeError, renderDecodeError)
import System.OsPath

import Core.Model.ContextName
import Core.Model.InventoryVariable
import Core.Model.ServiceName
import Core.Validation

data CLIErrorType
  = ParseError
  | FileDoesNotExist
  | GraphValidationError
  | UnkownContextFilter
  | VariableNotFound
  | CiliumValidationError
  deriving stock (Eq, Show, Ord, Enum, Bounded)

instance Display CLIErrorType where
  displayBuilder = \case
    ParseError -> "Parse error"
    FileDoesNotExist -> "File does not exist"
    GraphValidationError -> "Graph validation error"
    UnkownContextFilter -> "Unknown context filter"
    VariableNotFound -> "Variable not found"
    CiliumValidationError -> "Cilium validation error"

errorCodeFromType :: CLIErrorType -> ErrorCode
errorCodeFromType = \case
  ParseError -> ErrorCode 234
  FileDoesNotExist -> ErrorCode 100
  GraphValidationError -> ErrorCode 241
  UnkownContextFilter -> ErrorCode 154
  VariableNotFound -> ErrorCode 523
  CiliumValidationError -> ErrorCode 310

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

-- | Build a 'CLIError', deriving the error code from the error type
mkError :: CLIErrorType -> Text -> CLIError
mkError errorType errorMessage =
  CLIError {errorType, errorCode = errorCodeFromType errorType, errorMessage}

noFileAtProvidedLocation :: OsPath -> CLIError
noFileAtProvidedLocation path =
  mkError FileDoesNotExist $
    "Could not find file at " <> T.show path <> "."

kdlParseError :: OsPath -> DecodeError -> CLIError
kdlParseError path decodeError =
  mkError ParseError $
    "Could not parse file " <> T.show path <> ". Got the following error: " <> renderDecodeError decodeError

graphValidationError :: ValidationError -> CLIError
graphValidationError validationError =
  mkError GraphValidationError $
    T.show validationError

unknownContextFilter :: ContextName -> CLIError
unknownContextFilter contextFilter =
  mkError UnkownContextFilter $
    "Could not find context " <> display contextFilter <> " to filter services."

variableNotFound :: VariableName -> CLIError
variableNotFound expectedVariable =
  mkError VariableNotFound $
    "Could not find a definition for " <> display expectedVariable <> " in provided inventories."

missingConnectionTarget :: ServiceName -> ServiceName -> CLIError
missingConnectionTarget source target =
  mkError CiliumValidationError $
    "Service " <> display source <> " connects to undeclared service " <> display target <> "."

unreachableConnectionTarget :: ServiceName -> ServiceName -> CLIError
unreachableConnectionTarget source target =
  mkError CiliumValidationError $
    "Service "
      <> display source
      <> " cannot reach service "
      <> display target
      <> ": it has no fqdn and is not in the same context. Cilium needs either a shared context or an fqdn to emit an egress rule."

missingCidrSetTarget :: ServiceName -> Text -> CLIError
missingCidrSetTarget source target =
  mkError CiliumValidationError $
    "Service " <> display source <> " connects to undeclared cidr-set " <> target <> "."

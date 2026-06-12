{-# LANGUAGE RecordWildCards #-}

-- |
--   CLI Errors are given three-digit codes to help with referencing in search engines (especially tickets) and messaging applications.
--   These numbers are picked at random.
module CLI.Error where

import Data.List.NonEmpty (NonEmpty)
import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import KDL (DecodeError, renderDecodeError)
import System.OsPath
import Validation (Validation (..))

import Core.Model.ContextName
import Core.Model.InventoryVariable
import Core.Validation
import Render.Cilium.Resolved (ResolutionError (..))
import Render.Puml.C4Container.Types (InvalidPumlPropError (..), PumlError (..), UnknownPumlPropError (..))
import Render.Puml.PumlType (knownPumlProps)

data CLIErrorType
  = ParseError
  | FileDoesNotExist
  | GraphValidationError
  | UnknownContextFilter
  | VariableNotFound
  | CiliumValidationError
  | PumlValidationError
  deriving stock (Eq, Show, Ord, Enum, Bounded)

instance Display CLIErrorType where
  displayBuilder = \case
    ParseError -> "Parse error"
    FileDoesNotExist -> "File does not exist"
    GraphValidationError -> "Graph validation error"
    UnknownContextFilter -> "Unknown context filter"
    VariableNotFound -> "Variable not found"
    CiliumValidationError -> "Cilium validation error"
    PumlValidationError -> "Puml validation error"

errorCodeFromType :: CLIErrorType -> ErrorCode
errorCodeFromType = \case
  ParseError -> ErrorCode 234
  FileDoesNotExist -> ErrorCode 100
  GraphValidationError -> ErrorCode 241
  UnknownContextFilter -> ErrorCode 154
  VariableNotFound -> ErrorCode 523
  CiliumValidationError -> ErrorCode 310
  PumlValidationError -> ErrorCode 412

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

-- | Throw accumulated validation errors as 'CLIError's, or return the
-- validated value. Centralises the Failure -> throwError / Success -> pure
-- pattern shared by the render drivers.
throwOnFailure
  :: Error (NonEmpty CLIError) :> es
  => (e -> CLIError)
  -> Validation (NonEmpty e) a
  -> Eff es a
throwOnFailure toError = \case
  Failure errors -> Error.throwError (fmap toError errors)
  Success a -> pure a

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
  mkError UnknownContextFilter $
    "Could not find context " <> display contextFilter <> " to filter services."

variableNotFound :: VariableName -> CLIError
variableNotFound expectedVariable =
  mkError VariableNotFound $
    "Could not find a definition for " <> display expectedVariable <> " in provided inventories."

pumlValidationError :: PumlError -> CLIError
pumlValidationError = \case
  InvalidPumlProp InvalidPumlPropError {name, propKey, providedValue, supportedValues} ->
    mkError PumlValidationError $
      display name
        <> " has unknown value for "
        <> propKey
        <> ": "
        <> providedValue
        <> ". Supported values are "
        <> T.intercalate ", " supportedValues
  UnknownPumlProp UnknownPumlPropError {name, propKey} ->
    mkError PumlValidationError $
      display name
        <> " has unknown puml prop: "
        <> propKey
        <> ". Known props are "
        <> T.intercalate ", " (Set.toList knownPumlProps)

resolutionError :: ResolutionError -> CLIError
resolutionError =
  mkError CiliumValidationError . \case
    MissingService source target ->
      "Service " <> display source <> " connects to undeclared service " <> display target <> "."
    UnreachableService source target ->
      "Service "
        <> display source
        <> " cannot reach service "
        <> display target
        <> ": it has no fqdn and is not in the same context. Cilium needs either a shared context or an fqdn to emit an egress rule. Note that each context is a separate cluster: a parent and a nested sub-context are not the same context, so nesting alone does not make services reachable."
    MissingCidrSet source target ->
      "Service " <> display source <> " connects to undeclared cidr-set " <> target <> "."
    MissingEntity source target ->
      "Service " <> display source <> " accesses undeclared entity " <> display target <> "."

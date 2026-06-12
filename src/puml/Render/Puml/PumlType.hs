module Render.Puml.PumlType
  ( PumlType (..)
  , PropError (..)
  , pumlContainerMacro
  , pumlExternalContainerMacro
  , defaultPumlType
  , parseEnumProp
  , parsePumlType
  , knownPumlProps
  , validatePumlProps
  ) where

import Data.Either.Extra
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

-- | The kind of C4 container a service should be rendered as.
data PumlType
  = PumlQueue
  | PumlDatabase
  | PumlService
  deriving stock (Eq, Show, Ord, Enum, Bounded)

instance Display PumlType where
  displayBuilder = \case
    PumlQueue -> "queue"
    PumlDatabase -> "database"
    PumlService -> "service"

pumlContainerMacro :: PumlType -> Text
pumlContainerMacro = \case
  PumlQueue -> "ContainerQueue"
  PumlDatabase -> "ContainerDb"
  PumlService -> "Container"

pumlExternalContainerMacro :: PumlType -> Text
pumlExternalContainerMacro pumlType = pumlContainerMacro pumlType <> "_Ext"

pumlTypes :: Map Text PumlType
pumlTypes =
  [minBound .. maxBound]
    <&> (\pumlType -> (display pumlType, pumlType))
    & Map.fromList

defaultPumlType :: PumlType
defaultPumlType = PumlService

data PropError = PropError
  { propKey :: Text
  , providedValue :: Text
  , supportedValues :: List Text
  }
  deriving stock (Eq, Show, Ord)

parseEnumProp :: Text -> Map Text a -> a -> Map Text Text -> Either PropError a
parseEnumProp key valueMap def props =
  case Map.lookup key props of
    Nothing -> Right def
    Just providedValue ->
      maybeToEither
        (PropError key providedValue (Map.keys valueMap))
        (Map.lookup providedValue valueMap)

pumlTypeKey :: Text
pumlTypeKey = "puml:type"

-- | Parsing of a PUML type in the `puml:type` format.
-- >>> parsePumlType (Map.fromList [("puml:type", "database")])
-- Right PumlDatabase
parsePumlType :: Map Text Text -> Either PropError PumlType
parsePumlType = parseEnumProp pumlTypeKey pumlTypes defaultPumlType

knownPumlProps :: Set Text
knownPumlProps = Set.fromList [pumlTypeKey]

validatePumlProps :: Map Text Text -> Either Text Unit
validatePumlProps props =
  case List.find (`Set.notMember` knownPumlProps) pumlKeys of
    Just unknownKey -> Left unknownKey
    Nothing -> Right ()
  where
    pumlKeys = filter ("puml:" `T.isPrefixOf`) (Map.keys props)

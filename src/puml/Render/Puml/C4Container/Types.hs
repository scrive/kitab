{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Render.Puml.C4Container.Types
  ( C4Container (..)
  , ServiceTree (..)
  , PumlError (..)
  , InvalidPumlPropError (..)
  , UnknownPumlPropError (..)
  , mkC4ContainerAlias
  , toC4Container
  , buildServiceTree
  ) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful
import Effectful.State.Static.Local (State)
import Effectful.State.Static.Local qualified as State
import Optics.Core
import Prettyprinter

import Core.Model.CIDRSet
import Core.Model.ContextName
import Core.Model.EntityName
import Core.Model.Reference
import Core.Model.Service
import Core.Model.ServiceName
import Render.Puml.PumlType (PropError (..), PumlType, defaultPumlType, parsePumlType, validatePumlProps)

newtype C4ContainerAlias = C4ContainerAlias Text
  deriving newtype (Eq, Show, Ord, Pretty)

mkC4ContainerAlias :: Text -> C4ContainerAlias
mkC4ContainerAlias input =
  input
    & T.replace "-" "_"
    & T.replace " " "_"
    & C4ContainerAlias

data C4Container = C4Container
  { alias :: C4ContainerAlias
  , name :: Text
  , hierarchy :: List ContextName
  , pumlType :: PumlType
  }
  deriving stock (Eq, Show, Ord)

-- | A known prop carries a value the renderer does not accept.
data InvalidPumlPropError = InvalidPumlPropError
  { name :: Text
  , propKey :: Text
  , providedValue :: Text
  , supportedValues :: List Text
  }
  deriving stock (Eq, Show, Ord)

data UnknownPumlPropError = UnknownPumlPropError
  { name :: Text
  , propKey :: Text
  }
  deriving stock (Eq, Show, Ord)

data PumlError
  = InvalidPumlProp InvalidPumlPropError
  | UnknownPumlProp UnknownPumlPropError
  deriving stock (Eq, Show, Ord)

toC4Container
  :: Map ContextName (List ContextName)
  -- ^ Maps each context to its full hierarchy (root..self), so nested
  -- contexts render as nested boundaries. See 'contextHierarchies'.
  -> Map ServiceName (ServiceInfo var)
  -> Map Text (CIDRSet var)
  -> Reference
  -> Either PumlError C4Container
toC4Container contextHierarchies serviceIndex cidrIndex = \case
  ServiceRef serviceName@(ServiceName name) ->
    let mServiceInfo = Map.lookup serviceName serviceIndex
        hierarchy = expandContext contextHierarchies $ mServiceInfo ^? _Just % #serviceContext % _Just
        rendererProps = maybe Map.empty (.rendererProps) mServiceInfo
    in buildValidatedContainer name hierarchy rendererProps
  EntityRef (EntityName name) ->
    Right $ defaultContainer name []
  ToolRef mContext (ServiceName serviceName) name ->
    Right $ defaultContainer name (expandContext contextHierarchies mContext <> [ContextName serviceName])
  CIDRRef (CIDRConnection cidrSetName) ->
    let mCidrSet = Map.lookup cidrSetName cidrIndex
        rendererProps = maybe Map.empty (.rendererProps) mCidrSet
        hierarchy = expandContext contextHierarchies (mCidrSet >>= (.context))
    in buildValidatedContainer cidrSetName hierarchy rendererProps

-- | Expand a context reference into its full hierarchy path. Falls back to a
-- singleton path for contexts that were never declared (e.g. referenced only
-- via @in-context@), and the empty path when a reference has no context.
expandContext :: Map ContextName (List ContextName) -> Maybe ContextName -> List ContextName
expandContext contextHierarchies = \case
  Nothing -> []
  Just contextName -> Map.findWithDefault [contextName] contextName contextHierarchies

defaultContainer :: Text -> List ContextName -> C4Container
defaultContainer name hierarchy =
  C4Container {alias = mkC4ContainerAlias name, name, hierarchy, pumlType = defaultPumlType}

buildValidatedContainer :: Text -> List ContextName -> Map Text Text -> Either PumlError C4Container
buildValidatedContainer name hierarchy rendererProps =
  case validatePumlProps rendererProps of
    Left unknownKey -> Left $ UnknownPumlProp UnknownPumlPropError {name, propKey = unknownKey}
    Right () ->
      case parsePumlType rendererProps of
        Left PropError {propKey, providedValue, supportedValues} ->
          Left $ InvalidPumlProp InvalidPumlPropError {name, propKey, providedValue, supportedValues}
        Right pumlType -> Right C4Container {alias = mkC4ContainerAlias name, name, hierarchy, pumlType}

data ServiceTree = ServiceTree
  { leaves :: List C4Container
  , subTrees :: Map ContextName ServiceTree
  }
  deriving stock (Eq, Show, Ord)

emptyServiceTree :: ServiceTree
emptyServiceTree =
  ServiceTree
    { leaves = []
    , subTrees = Map.empty
    }

-- | Build a service tree based on a list of services.
--  This function sorts the services internally.
buildServiceTree :: List C4Container -> ServiceTree
buildServiceTree services =
  services
    & List.sortOn (.hierarchy)
    & \sortedServices ->
      runPureEff $ State.evalState sortedServices (buildNode [] emptyServiceTree)

-- | We want to iterate over the list of 'C4Container' with the following decision tree:
--   * If the list has a service in it, we process it:
--     * If the service's hierarchy is the same as the current @path@ we add it
--        to the current node's leaves
--     * If the service's hierarchy is a prefix of the current @path@ we compute
--        the key for the service based.
--   * If the list is empty, or the service's hierarchy is not in the path,
--      then we return the accumulated 'ServiceTree', thus ending the recursion.
buildNode
  :: State (List C4Container) :> es
  => List ContextName
  -> ServiceTree
  -> Eff es ServiceTree
buildNode path st =
  sPeek >>= \case
    Nothing -> pure st
    Just service
      | service.hierarchy == path ->
          sTail >> buildNode path st {leaves = service : st.leaves}
      | path `List.isPrefixOf` service.hierarchy -> do
          let pp = take (length path + 1) service.hierarchy
          newSubTree <- buildNode pp emptyServiceTree
          let subTrees' = Map.insert (last pp) newSubTree st.subTrees
          buildNode path st {subTrees = subTrees'}
      | otherwise -> pure st

sPeek :: State (List C4Container) :> es => Eff es (Maybe C4Container)
sPeek = State.gets listToMaybe

sTail :: State (List C4Container) :> es => Eff es Unit
sTail = State.modify @(List C4Container) (List.drop 1)

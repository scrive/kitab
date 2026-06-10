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
  { serviceName :: ServiceName
  , propKey :: Text
  , providedValue :: Text
  , supportedValues :: List Text
  }
  deriving stock (Eq, Show, Ord)

-- | A @puml:@ prop key the renderer does not understand (e.g. a typo).
data UnknownPumlPropError = UnknownPumlPropError
  { serviceName :: ServiceName
  , propKey :: Text
  }
  deriving stock (Eq, Show, Ord)

-- | A renderer-time validation failure on a service's @puml:@-namespaced props.
data PumlError
  = InvalidPumlProp InvalidPumlPropError
  | UnknownPumlProp UnknownPumlPropError
  deriving stock (Eq, Show, Ord)

toC4Container :: Map ServiceName (ServiceInfo var) -> Reference -> Either PumlError C4Container
toC4Container serviceIndex = \case
  ServiceRef serviceName@(ServiceName name) ->
    let alias = mkC4ContainerAlias name
        mServiceInfo = Map.lookup serviceName serviceIndex
        hierarchy = maybeToList $ mServiceInfo ^? _Just % #serviceContext % _Just
        rendererProps = maybe Map.empty (.rendererProps) mServiceInfo
    in case validatePumlProps rendererProps of
         Left unknownKey -> Left $ UnknownPumlProp UnknownPumlPropError {serviceName, propKey = unknownKey}
         Right () ->
           case parsePumlType rendererProps of
             Left PropError {propKey, providedValue, supportedValues} ->
               Left $ InvalidPumlProp InvalidPumlPropError {serviceName, propKey, providedValue, supportedValues}
             Right pumlType -> Right C4Container {alias, name, hierarchy, pumlType}
  EntityRef (EntityName name) ->
    let alias = mkC4ContainerAlias name
        hierarchy = []
    in Right C4Container {alias, name, hierarchy, pumlType = defaultPumlType}
  ToolRef mContext (ServiceName serviceName) name ->
    let alias = mkC4ContainerAlias name
        hierarchy = maybeToList mContext <> [ContextName serviceName]
    in Right C4Container {alias, name, hierarchy, pumlType = defaultPumlType}
  CIDRRef (CIDRConnection name) ->
    let alias = mkC4ContainerAlias name
        hierarchy = []
    in Right C4Container {alias, name, hierarchy, pumlType = defaultPumlType}

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

-- |  Build a service tree based on a list of services.
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

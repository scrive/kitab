{-# LANGUAGE OverloadedLabels #-}

module Render.Puml.C4Container.Types
  ( C4Container (..)
  , ServiceTree (..)
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
  }
  deriving stock (Eq, Show, Ord)

toC4Container :: Map ServiceName (ServiceInfo var) -> Reference -> C4Container
toC4Container serviceIndex = \case
  ServiceRef (ServiceName name) ->
    let alias = mkC4ContainerAlias name
        mServiceInfo = Map.lookup (ServiceName name) serviceIndex
        hierarchy = maybeToList $ mServiceInfo ^? _Just % #serviceContext % _Just
    in C4Container {alias, name, hierarchy}
  EntityRef (EntityName name) ->
    let alias = mkC4ContainerAlias name
        hierarchy = []
    in C4Container {alias, name, hierarchy}
  ToolRef mContext (ServiceName serviceName) name ->
    let alias = mkC4ContainerAlias name
        hierarchy = maybeToList mContext <> [ContextName serviceName]
    in C4Container {alias, name, hierarchy}

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

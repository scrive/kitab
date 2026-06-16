module Core.Graph
  ( PreparedModel (..)
  , buildPreparedModel
  , buildGraph
  , buildServiceIndex
  , buildEntityIndex
  , buildCidrIndex
  ) where

import Algebra.Graph.Labelled (Graph)
import Algebra.Graph.Labelled qualified as Graph
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Data.Void
import Validation

import Core.Model.CIDRSet
import Core.Model.Entity
import Core.Model.EntityName
import Core.Model.Reference
import Core.Model.Service
import Core.Model.ServiceContext
import Core.Model.ServiceName
import Core.Validation

-- | The fully resolved, validated-elsewhere model handed to a renderer: the
-- connection graph plus the lookup indices and declared contexts every output
-- format draws from.
data PreparedModel = PreparedModel
  { graph :: Graph (List ConnectionType) Reference
  , services :: List (Service Void)
  , serviceIndex :: Map ServiceName (ServiceInfo Void)
  , entityIndex :: Map EntityName EntityInfo
  , cidrIndex :: Map Text (CIDRSet Void)
  , contexts :: List ServiceContext
  }

buildPreparedModel
  :: List (Service Void)
  -> List Entity
  -> List (CIDRSet Void)
  -> List ServiceContext
  -> Validation (NonEmpty ValidationError) PreparedModel
buildPreparedModel services entities cidrs contexts = do
  let graph = buildGraph services entities
  case checkGraph graph of
    Failure err -> Failure err
    Success _ ->
      Success $
        PreparedModel
          { graph
          , services
          , serviceIndex = buildServiceIndex services
          , entityIndex = buildEntityIndex entities
          , cidrIndex = buildCidrIndex cidrs
          , contexts
          }

-- | Reusable helper to make generic the assembly of an index
-- according to two accessors, the first producing a key and the second one
-- producing a value out of the list of elements being passed.
buildIndex :: Ord key => (a -> key) -> (a -> value) -> List a -> Map key value
buildIndex key val = List.foldr (\x -> Map.insert (key x) (val x)) Map.empty

buildServiceIndex :: List (Service Void) -> Map ServiceName (ServiceInfo Void)
buildServiceIndex = buildIndex (.serviceName) (.serviceInfo)

buildEntityIndex :: List Entity -> Map EntityName EntityInfo
buildEntityIndex = buildIndex (.entityName) (.entityInfo)

buildCidrIndex :: List (CIDRSet Void) -> Map Text (CIDRSet Void)
buildCidrIndex = buildIndex (.setName) identity

buildGraph :: List (Service var) -> List Entity -> Graph (List ConnectionType) Reference
buildGraph services entities =
  Graph.overlays $
    Graph.edges (concatMap serviceEdges services)
      : [Graph.vertex (EntityRef entity.entityName) | entity <- entities]

-- | Every outgoing edge of a single service: connections to other services,
-- entity accesses, external tool calls and CIDR connections.
serviceEdges :: Service var -> List (Tuple3 (List ConnectionType) Reference Reference)
serviceEdges service =
  concat
    [ [(List.singleton connection.connectionType, source, ServiceRef connection.connectionWith) | connection <- service.serviceConnections]
    , [(List.singleton HTTPS, source, EntityRef access.accessTarget) | access <- service.entityAccesses]
    , [(List.singleton ExternalTool, source, ToolRef service.serviceName call) | call <- service.toolCalls]
    , [(List.singleton Network, source, CIDRRef call) | call <- service.cidrConnections]
    ]
  where
    source = ServiceRef service.serviceName

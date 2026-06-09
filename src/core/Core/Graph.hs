module Core.Graph
  ( buildGraph
  , buildServiceIndex
  , buildEntityIndex
  , buildCidrIndex
  ) where

import Algebra.Graph.Labelled (Graph)
import Algebra.Graph.Labelled qualified as Graph
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Void

import Core.Model.CIDRSet
import Core.Model.Entity
import Core.Model.EntityName
import Core.Model.Reference
import Core.Model.Service
import Core.Model.ServiceName

buildIndex :: Ord k => (a -> k) -> (a -> v) -> List a -> Map k v
buildIndex key val = foldr (\x -> Map.insert (key x) (val x)) Map.empty

buildServiceIndex :: List (Service Void) -> Map ServiceName (ServiceInfo Void)
buildServiceIndex = buildIndex (.serviceName) (.serviceInfo)

buildEntityIndex :: List Entity -> Map EntityName EntityInfo
buildEntityIndex = buildIndex (.entityName) (.entityInfo)

buildCidrIndex :: List (CIDRSet Void) -> Map Text (CIDRSet Void)
buildCidrIndex = buildIndex (.setName) identity

buildGraph :: List (Service var) -> List Entity -> Graph (List ConnectionType) Reference
buildGraph services entities =
  let serviceGraph =
        foldr
          ( \service acc ->
              let incomingService = ServiceRef service.serviceName
                  serviceConnectionsGraph =
                    Graph.edges [(List.singleton connection.connectionType, incomingService, ServiceRef connection.connectionWith) | connection <- service.serviceConnections]
                  entityConnectionsGraph =
                    Graph.edges [(List.singleton HTTPS, incomingService, EntityRef access.accessTarget) | access <- service.entityAccesses]
                  toolConnectionsGraph =
                    Graph.edges [(List.singleton ExternalTool, incomingService, ToolRef service.serviceInfo.serviceContext service.serviceName call) | call <- service.toolCalls]
              in Graph.overlays [serviceConnectionsGraph, entityConnectionsGraph, toolConnectionsGraph, acc]
          )
          Graph.empty
          services
  in foldr
       ( \entity ->
           let incomingEntity = EntityRef entity.entityName
               builtGraph = Graph.vertex incomingEntity
           in Graph.overlay builtGraph
       )
       serviceGraph
       entities

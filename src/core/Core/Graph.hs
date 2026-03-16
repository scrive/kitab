module Core.Graph
  ( buildGraph
  , buildServiceIndex
  , buildEntityIndex
  ) where

import Algebra.Graph.Labelled (Graph)
import Algebra.Graph.Labelled qualified as Graph
import Data.List qualified as List
import Data.Map.Strict qualified as Map

import Core.Model.Entity
import Core.Model.EntityName
import Core.Model.Reference
import Core.Model.Service
import Core.Model.ServiceName

buildServiceIndex :: List (Service var) -> Map ServiceName (ServiceInfo var)
buildServiceIndex =
  foldr
    ( \Service {serviceName, serviceInfo} ->
        Map.insert serviceName serviceInfo
    )
    Map.empty

buildEntityIndex :: List Entity -> Map EntityName EntityInfo
buildEntityIndex =
  foldr
    ( \Entity {entityName, entityInfo} ->
        Map.insert entityName entityInfo
    )
    Map.empty

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
              in Graph.overlays [serviceConnectionsGraph, entityConnectionsGraph, acc]
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

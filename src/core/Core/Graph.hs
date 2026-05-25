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

buildServiceIndex :: List (Service Void) -> Map ServiceName (ServiceInfo Void)
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

buildCidrIndex :: List (CIDRSet Void) -> Map Text (CIDRSet Void)
buildCidrIndex =
  foldr
    ( \cidrSet@CIDRSet {setName} ->
        Map.insert setName cidrSet
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

module Core.Graph
  ( buildGraph
  , buildIndex
  ) where

import Algebra.Graph.Labelled (Graph)
import Algebra.Graph.Labelled qualified as Graph
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Core.Model.Entity
import Core.Model.Reference
import Core.Model.Service
import Core.Model.ServiceName

buildIndex :: List Service -> Map ServiceName ServiceInfo
buildIndex =
  foldr
    ( \Service {serviceName, serviceInfo} ->
        Map.insert serviceName serviceInfo
    )
    Map.empty

buildGraph :: List Service -> List Entity -> Graph (List ConnectionType) Reference
buildGraph services entities =
  let serviceGraph =
        foldr
          ( \service ->
              let incomingService = ServiceRef service.serviceName
                  builtGraph = Graph.edges [(List.singleton connection.connectionType, incomingService, ServiceRef connection.connectionWith) | connection <- service.serviceConnections]
              in Graph.overlay builtGraph
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

module Core.Graph
  ( buildGraph
  , buildIndex
  ) where

import Algebra.Graph.Labelled (Graph)
import Algebra.Graph.Labelled qualified as Graph
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Core.Model.Service

buildIndex :: List Service -> Map ServiceReference ServiceInfo
buildIndex =
  foldr
    ( \Service{serviceName, serviceInfo} ->
        let reference = ServiceReference serviceName serviceInfo.serviceContext
        in Map.insert reference serviceInfo
    )
    Map.empty

buildGraph :: List Service -> Graph (List ConnectionType) ServiceReference
buildGraph =
  foldr
    ( \service ->
        let incomingService = ServiceReference service.serviceName service.serviceInfo.serviceContext
            builtGraph = Graph.edges [(List.singleton connection.connectionType, incomingService, connection.connectionWith) | connection <- service.connections]
        in Graph.overlay builtGraph
    )
    Graph.empty

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
import Core.Model.ServiceName

buildIndex :: List Service -> Map ServiceName ServiceInfo
buildIndex =
  foldr
    ( \Service {serviceName, serviceInfo} ->
        Map.insert serviceName serviceInfo
    )
    Map.empty

buildGraph :: List Service -> Graph (List ConnectionType) ServiceName
buildGraph =
  foldr
    ( \service ->
        let incomingService = service.serviceName
            builtGraph = Graph.edges [(List.singleton connection.connectionType, incomingService, connection.connectionWith) | connection <- service.connections]
        in Graph.overlay builtGraph
    )
    Graph.empty

module Core.Graph
  ( buildGraph
  , buildIndex
  ) where

import Algebra.Graph.Labelled (Graph)
import Algebra.Graph.Labelled qualified as Graph
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Core.Model.Service

buildIndex :: List Service -> Map ServiceName ServiceInfo
buildIndex =
  foldr
    ( \service ->
        Map.insert service.serviceName service.serviceInfo
    )
    Map.empty

buildGraph :: List Service -> Graph (List ConnectionType) ServiceName
buildGraph =
  foldr
    ( \service ->
        let builtGraph = Graph.edges [(List.singleton connection.connectionType, service.serviceName, connection.connectionWith) | connection <- service.connections]
        in Graph.overlay builtGraph
    )
    Graph.empty

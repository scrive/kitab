module Core.Validation where

import Data.List
import Data.List.NonEmpty (NonEmpty)
import Validation

import Algebra.Graph.Labelled
import Core.Model
import Data.Foldable
import Data.Function

data ValidationError
  = Asymmetric ServiceName ServiceName
  | Mismatched ServiceName ServiceName ConnectionType ConnectionType
  | -- | A service is calling itself with two different connection types
    Parallel ServiceName ServiceName (List ConnectionType)
    -- | A service declares a connection to itself
  | SelfReferential ServiceName
  deriving stock (Eq, Ord, Show)

checkGraph
  :: Graph (List ConnectionType) ServiceName
  -> Validation (NonEmpty ValidationError) ()
checkGraph graph =
  sequenceA_
    [ checkParallelEdges graph
    -- , checkAsymmetric
    -- , checkMismatched
    -- , checkSelfReferential
    ]

-- >>> checkParallelEdges (build [Service "A" [(Connection "B" HTTPS), (Connection "B" FunctionCall)]])
-- Failure (Parallel "A" "B" [HTTPS,FunctionCall] :| [])
checkParallelEdges
  :: Graph (List ConnectionType) ServiceName
  -> Validation (NonEmpty ValidationError) ()
checkParallelEdges graph =
  let parallelGraphEdges =
        graph
          & edgeList
          & filter (\(connTypes, _, _) -> length connTypes > 1)
      reportParallelEdge (graphEdges, source, destination) = failure (Parallel source destination graphEdges)
  in for_ parallelGraphEdges reportParallelEdge

module Core.Validation where

import Algebra.Graph.Labelled (Graph)
import Algebra.Graph.Labelled qualified as Graph
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Validation

import Core.Model.Reference
import Core.Model.Service

data ValidationError
  = Asymmetric Reference Reference
  | -- | Two services declare different ways of reaching each-other.
    Mismatched
      ( Tuple2
          (Tuple3 Reference Reference ConnectionType)
          (Tuple3 Reference Reference ConnectionType)
      )
  | -- | A service declares two different ways to reach another service.
    Parallel Reference Reference (List ConnectionType)
  | -- | A service declares a connection to itself.
    SelfReferential Reference
  deriving stock (Eq, Ord, Show)

checkGraph
  :: Graph (List ConnectionType) Reference
  -> Validation (NonEmpty ValidationError) Unit
checkGraph graph =
  sequenceA_
    [ checkParallelEdges graph
    , checkSelfReferential graph
    , checkMismatched graph
    ]

-- >>> checkParallelEdges (build [Service "A" [(Connection "B" HTTPS), (Connection "B" FunctionCall)]])
-- Failure (Parallel "A" "B" [HTTPS,FunctionCall] :| [])
checkParallelEdges
  :: Graph (List ConnectionType) Reference
  -> Validation (NonEmpty ValidationError) Unit
checkParallelEdges graph =
  let parallelGraphEdges =
        graph
          & Graph.edgeList
          & filter (\(connTypes, _, _) -> length connTypes > 1)
  in for_ parallelGraphEdges (\(graphEdges, source, destination) -> failure (Parallel source destination graphEdges))

-- >>> checkSelfReferential (build [Service "A" [(Connection "A" HTTPS)]])
-- Failure (SelfReferential "A" :| [])
checkSelfReferential
  :: Graph (List ConnectionType) Reference
  -> Validation (NonEmpty ValidationError) Unit
checkSelfReferential graph =
  let selfReferentialEdges =
        graph
          & Graph.edgeList
          & filter (\(_, source, destination) -> source == destination)
  in for_ selfReferentialEdges (\(_, source, _) -> failure (SelfReferential source))

-- >>> checkMismatched (build [Service "A" [(Connection "B" HTTPS)], Service "B" [Connection "A" FunctionCall]])
-- Failure (Mismatched (("A","B",HTTPS),("B","A",FunctionCall)) :| [])
checkMismatched
  :: Graph (List ConnectionType) Reference
  -> Validation (NonEmpty ValidationError) Unit
checkMismatched graph =
  let am =
        graph
          & Graph.edgeList
          & AM.edges
          & AM.adjacencyMap
      getLabels source destination = Map.findWithDefault mempty destination (Map.findWithDefault Map.empty source am)
      faultyPairs =
        [ (edgeSD, edgeDS)
        | -- Iterate over every node 'u' and its neighbors 'v'
        (source, targets) <- Map.toList am
        , (destination, labelsSD) <- Map.toList targets
        , -- Canonical order: only check A vs B (ignore B vs A) to prevent duplicates
        source < destination
        , -- Look up the opposite direction
        let labelsDS = getLabels destination source
        , not (null labelsDS) -- Ensure opposite edges actually exist
        , -- "Unpack" the list of labels to treat them as individual edge instances
        labelSD <- labelsSD
        , -- Iterate through opposite labels and check for mismatch
        labelDS <- labelsDS
        , labelSD /= labelDS
        , -- Reconstruct the RawEdge for the report
        let edgeSD = (source, destination, labelSD)
        , let edgeDS = (destination, source, labelDS)
        ]
  in for_ faultyPairs (failure . Mismatched)

module Render.JsonGraph.Types where

import Data.Map.Strict (Map)
import Deriving.Aeson

data JsonGraph = JsonGraph
  { id :: Text
  , _type :: Text
  , directed :: Bool
  , nodes :: Map Text GraphNode
  , edges :: List GraphEdge
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON) via GraphJSON JsonGraph

data GraphNode = GraphNode
  { label :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON) via GraphJSON GraphNode

data GraphEdge = GraphEdge
  { source :: Text
  , relation :: Text
  , target :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON) via GraphJSON GraphEdge

type GraphJSON a =
  CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "_", CamelToSnake]] a

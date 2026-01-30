module Core.Model where

import Algebra.Graph.Labelled (Graph)
import Algebra.Graph.Labelled qualified as Graph
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String (IsString)
import Prettyprinter

newtype ServiceName = ServiceName Text
  deriving newtype (Eq, Ord, Show, IsString, Pretty, Display)

data ConnectionType
  = HTTPS
  | FunctionCall
  deriving stock (Eq, Show, Ord)

instance Pretty ConnectionType where
  pretty = \case
    HTTPS -> "HTTPS"
    FunctionCall -> "Function call"

data Service = Service
  { serviceName :: ServiceName
  , serviceInfo :: ServiceInfo
  , connections :: List Connection
  }
  deriving stock (Eq, Show, Ord)

data ServiceInfo = ServiceInfo
  { serviceFqdn :: Maybe Text
  -- ^ Fqdn is a Cilium thing.
  }
  deriving stock (Eq, Show, Ord)

defaultServiceInfo :: ServiceInfo
defaultServiceInfo =
  ServiceInfo
    { serviceFqdn = Nothing
    }

data Connection = Connection
  { connectionWith :: ServiceName
  , connectionType :: ConnectionType
  }
  deriving stock (Eq, Show, Ord)

buildIndex :: List Service -> Map ServiceName ServiceInfo
buildIndex =
  foldr
    ( \service ->
        Map.insert service.serviceName service.serviceInfo
    )
    Map.empty

build :: List Service -> Graph (List ConnectionType) ServiceName
build =
  foldr
    ( \service ->
        let builtGraph = Graph.edges [(List.singleton connection.connectionType, service.serviceName, connection.connectionWith) | connection <- service.connections]
        in Graph.overlay builtGraph
    )
    Graph.empty

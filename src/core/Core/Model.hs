module Core.Model where

import Algebra.Graph.Labelled (Graph)
import Algebra.Graph.Labelled qualified as Graph
import Data.List qualified as List
import Data.String (IsString)
import Prettyprinter

newtype ServiceName = ServiceName Text
  deriving newtype (Eq, Ord, Show, IsString, Pretty)

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
  , connections :: List Connection
  }
  deriving stock (Eq, Show, Ord)

data Connection = Connection
  { connectionWith :: ServiceName
  , connectionType :: ConnectionType
  }
  deriving stock (Eq, Show, Ord)

-- >>> let services = build [Service "A" ([Connection "B" HTTPS]), Service "B" ([Connection "A" HTTPS])]
-- >>> edgeList services
-- [(fromList [HTTPS],"A","B"),(fromList [HTTPS],"B","A")]
-- >>> vertexList services
-- ["A","B"]
-- >>> edgeList (build [Service "A" ([Connection "B" HTTPS, Connection "B" FunctionCall]), Service "B" ([Connection "A" HTTPS])])
-- [(fromList [HTTPS,FunctionCall],"A","B"),(fromList [HTTPS],"B","A")]
build :: List Service -> Graph (List ConnectionType) ServiceName
build =
  foldr
    ( \service ->
        let builtGraph = Graph.edges [(List.singleton connection.connectionType, service.serviceName, connection.connectionWith) | connection <- service.connections]
        in Graph.overlay builtGraph
    )
    Graph.empty

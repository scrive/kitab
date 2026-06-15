module Render.GEXF where

import Algebra.Graph.Labelled.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Data.Map.Strict qualified as Map
import Data.Text.Lazy qualified as TL
import Data.Version (showVersion)
import Data.Void
import Optics.Core hiding (element)
import Text.XML (Name, def, renderText)
import Text.XML.Stream.Render.Internal (RenderSettings (..))
import Text.XML.Writer

import Core.Model.Reference
import Core.Model.Service
import Core.Model.ServiceContext ()
import Core.Model.ServiceName
import Paths_kitab (version)
import Render.GEXF.Types

renderToGEXF
  :: Bool
  -> AdjacencyMap (List ConnectionType) Reference
  -> Map ServiceName (ServiceInfo Void)
  -> Text
renderToGEXF enableVersionStamp graph serviceIndex =
  TL.toStrict
    . renderText def {rsPretty = True}
    . documentA "gexf" ([("xmlns", "http://www.gexf.net/1.2draft"), ("version", "1.2")] <> versionStamp)
    $ do
      elementA "graph" [("mode", "static"), ("defaultedgetype", "directed")] $ do
        toXML defaultNodeAttributes
        element "nodes" (mapM_ renderNode (AM.vertexList graph))

        element "edges" $
          mapM_ renderEdge (zip [0 :: Word ..] validEdges)
  where
    validEdges = filter (\(e, _, _) -> e /= mempty) (AM.edgeList graph)

    renderNode :: Reference -> XML
    renderNode = \case
      ServiceRef serviceName ->
        let mServiceInfo = Map.lookup serviceName serviceIndex
            hierarchy = mServiceInfo ^? _Just % #serviceContext % _Just
        in toXML $ serviceToGexfNode serviceName hierarchy
      EntityRef entityRef -> toXML $ entityRefToNode entityRef
      ToolRef (ServiceName service) toolName ->
        toXML $ toolToGexfNode service toolName
      CIDRRef cidrConnection ->
        toXML $ cidrConnectionToGexfNode cidrConnection

    renderEdge :: Tuple2 Word (Tuple3 (List ConnectionType) Reference Reference) -> XML
    renderEdge (i, (e, u, v)) =
      toXML
        Edge
          { edgeId = EdgeId i
          , edgeSource = referenceNodeId u
          , edgeTarget = referenceNodeId v
          , edgeLabel = display (head e)
          }
    versionStamp :: List (Tuple2 Name Text)
    versionStamp =
      if enableVersionStamp
        then [("kitab:version", display (showVersion version))]
        else []

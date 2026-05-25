module Render.C4 where

import Algebra.Graph.Labelled.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Core.Model.ContextName
import Core.Model.Service
import Core.Model.ServiceContext
import Core.Model.ServiceName
import Render.C4.C4Service.Types

renderC4
  :: List ServiceContext
  -> AdjacencyMap [ConnectionType] C4Service
  -> Map ServiceName (List Text)
  -> Text
renderC4 contexts graph tools = renderStrict . layoutPretty defaultLayoutOptions $ pumlDoc
  where
    pumlDoc :: Doc ann
    pumlDoc =
      vsep
        [ "@startuml"
        , "!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Container.puml"
        , ""
        , "title System Architecture (C4 Container View)"
        , ""
        , "' --- Contexts ---"
        , prettyContexts contexts (AM.vertexList graph)
        , ""
        , "' --- Outer Systems ---"
        , prettyOutOfContext (AM.vertexList graph)
        , ""
        , "' --- Relationships ---"
        , vsep (map prettyEdge (AM.edgeList graph))
        , "@enduml"
        ]

prettySystemNode :: C4Service -> Doc ann
prettySystemNode C4Service {alias, name} =
  "System"
    <> tupled
      [ pretty alias
      , dquotes (pretty name)
      ]

prettyContainerNode :: Text -> Doc ann
prettyContainerNode name =
  "Container"
    <> tupled
      [ pretty name
      , dquotes (pretty name)
      ]

prettyEdge :: (List ConnectionType, C4Service, C4Service) -> Doc ann
prettyEdge (connTypes, from, to) =
  "Rel"
    <> tupled
      [ pretty from.alias
      , pretty to.alias
      , dquotes (pretty $ head connTypes)
      ]

prettyOutOfContext :: List C4Service -> Doc ann
prettyOutOfContext services =
  let docs =
        services
          & List.filter (\s -> null s.boundaryHierarchy)
          & List.map prettySystemNode
  in vsep docs

prettyContexts :: List ServiceContext -> List C4Service -> Doc ann
prettyContexts contexts services =
  let docs = flip List.map contexts $ \context ->
        let contextServices =
              services
                & List.filter (\s -> s.boundaryHierarchy == [context.contextName])
        in prettyServiceContext context contextServices
  in vsep docs

prettyServiceContext
  :: ServiceContext
  -> List C4Service
  -> Doc ann
prettyServiceContext serviceContext services =
  vsep
    [ "System_Boundary(c1," <> pretty serviceContext.contextName <> ") {"
    , indent 2 $ vsep (map prettySystemNode services)
    , "}"
    ]

prettyServiceContextWithTools
  :: ServiceContext
  -> List C4Service
  -> List Text
  -> Doc ann
prettyServiceContextWithTools serviceContext services serviceTools =
  vsep
    [ "Container_Boundary(" <> pretty serviceContext.contextName <> ", " <> pretty serviceContext.contextName <> ") {"
    , indent 2 $ vsep (map prettyContainerNode serviceTools)
    , "}"
    ]

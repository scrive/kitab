module Render.C4 where

import Algebra.Graph.Labelled.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Data.List qualified as List
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Core.Model.Service
import Core.Model.ServiceContext
import Render.C4.C4Service.Types

renderC4
  :: List ServiceContext
  -> AdjacencyMap [ConnectionType] C4Service
  -> Text
renderC4 contexts graph = renderStrict . layoutPretty defaultLayoutOptions $ pumlDoc
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

prettyNode :: C4Service -> Doc ann
prettyNode C4Service {alias, name} =
  "System"
    <> tupled
      [ pretty alias
      , dquotes (pretty name)
      ]

prettyEdge :: (List ConnectionType, C4Service, C4Service) -> Doc ann
prettyEdge (connTypes, from, to) =
  "Rel"
    <> tupled
      [ pretty from.alias
      , pretty to.alias
      , dquotes (pretty (connTypeLabel $ head connTypes))
      , dquotes (pretty $ head connTypes)
      ]

prettyOutOfContext :: List C4Service -> Doc ann
prettyOutOfContext services =
  let docs =
        services
          & List.filter (\s -> isNothing s.systemBoundary)
          & List.map prettyNode
  in vsep docs

prettyContexts :: List ServiceContext -> List C4Service -> Doc ann
prettyContexts contexts services =
  let docs = flip List.map contexts $ \context ->
        let contextServices = List.filter (\s -> s.systemBoundary == Just context.contextName) services
        in prettyContext context contextServices
  in vsep docs

prettyContext :: ServiceContext -> List C4Service -> Doc ann
prettyContext serviceContext services =
  vsep
    [ "System_Boundary(c1," <> pretty serviceContext.contextName <> ") {"
    , indent 2 $ vsep (map prettyNode services)
    , "}"
    ]

connTypeLabel :: ConnectionType -> Text
connTypeLabel = \case
  Network -> "Connects via"
  FunctionCall -> "using"

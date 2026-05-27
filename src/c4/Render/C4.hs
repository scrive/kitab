module Render.C4 (renderC4) where

import Algebra.Graph.Labelled.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Core.Model.ContextName
import Core.Model.Service
import Render.C4.C4Container.Types

renderC4
  :: AdjacencyMap (List ConnectionType) C4Container
  -> Text
renderC4 graph = renderStrict . layoutPretty defaultLayoutOptions $ pumlDoc
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
        , prettyContexts (AM.vertexList graph)
        , ""
        , "' --- Relationships ---"
        , vsep (map prettyEdge (AM.edgeList graph))
        , "@enduml"
        ]

prettyEdge :: Tuple3 (List ConnectionType) C4Container C4Container -> Doc ann
prettyEdge (connTypes, from, to) =
  case head connTypes of
    ExternalTool -> ""
    connType ->
      "Rel"
        <> tupled
          [ pretty from.alias
          , pretty to.alias
          , dquotes (pretty connType)
          ]

prettyContexts :: List C4Container -> Doc ann
prettyContexts serviceList =
  serviceList
    & buildServiceTree
    & serviceTreeToPuml

serviceTreeToPuml :: ServiceTree -> Doc ann
serviceTreeToPuml serviceTree =
  vsep
    ( List.map prettyContainerNode serviceTree.leaves
        <> List.map subTreeToPuml (Map.toList serviceTree.subTrees)
    )

subTreeToPuml :: Tuple2 ContextName ServiceTree -> Doc ann
subTreeToPuml (ContextName name, tree) =
  vsep
    [ "Container_Boundary(" <> pretty (mkC4ContainerAlias name) <> ", " <> pretty name <> ") {"
    , indent
        2
        ( vsep
            ( List.map prettyContainerNode tree.leaves
                <> List.map subTreeToPuml (Map.toList tree.subTrees)
            )
        )
    , "}"
    ]

prettyContainerNode :: C4Container -> Doc ann
prettyContainerNode service =
  "Container"
    <> tupled
      [ pretty service.alias
      , dquotes (pretty service.name)
      ]

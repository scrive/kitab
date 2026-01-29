module Render.C4 where

import Algebra.Graph.Labelled.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Core.Model
import Render.C4.Types

renderC4 :: AdjacencyMap [ConnectionType] C4Service -> Text
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
        , "' --- Systems ---"
        , vsep (map prettyNode (AM.vertexList graph))
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

prettyEdge :: ([ConnectionType], C4Service, C4Service) -> Doc ann
prettyEdge (connTypes, from, to) =
  "Rel"
    <> tupled
      [ pretty from.alias
      , pretty to.alias
      , dquotes (pretty (connTypeLabel $ head connTypes))
      , dquotes (pretty $ head connTypes)
      ]

connTypeLabel :: ConnectionType -> Text
connTypeLabel = \case
  HTTPS -> "Connects via"
  FunctionCall -> "using"

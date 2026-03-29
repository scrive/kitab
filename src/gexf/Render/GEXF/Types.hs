module Render.GEXF.Types where

import Data.List qualified as List
import Text.XML.Writer

import Core.Model.ContextName
import Core.Model.EntityName
import Core.Model.ServiceName

-- | Root GEXF element
data GexfDocument = GexfDocument Meta Graph
  deriving stock (Eq, Ord, Show)

newtype Label = Label Text
  deriving
    (Eq, Ord, Show, Display)
    via Text

newtype EdgeId = EdgeId Word
  deriving
    (Eq, Ord, Show, Display)
    via Word

newtype NodeId = NodeId Text
  deriving
    (Eq, Ord, Show, Display)
    via Text

-- | Metadata for the graph
data Meta = Meta
  { creator :: Text
  , description :: Text
  , lastModified :: Text
  }
  deriving stock (Eq, Ord, Show)

-- | Core graph structure
data Graph = Graph
  { edgeType :: EdgeType
  , mode :: Mode
  , attributes :: [Attributes]
  , nodes :: List Node
  , edges :: List Edge
  }
  deriving stock (Eq, Ord, Show)

data EdgeType = Directed | Undirected | Mutual
  deriving stock (Eq, Ord, Show)

data Mode = Static | Dynamic
  deriving stock (Eq, Ord, Show)

data Attributes = Attributes
  { attrClass :: AttrClass
  , attributeList :: List AttrDef
  }
  deriving stock (Eq, Ord, Show)

instance ToXML Attributes where
  toXML Attributes {attrClass, attributeList} =
    elementA
      "attributes"
      [("class", display attrClass)]
      (mapM_ toXML attributeList)

defaultNodeAttributes :: Attributes
defaultNodeAttributes =
  Attributes
    { attrClass = ClassNode
    , attributeList =
        [ contextAttrDef
        , contextIdAttrDef
        ]
    }

-- | Attribute definitions (schema)
data AttrDef = AttrDef
  { attrId :: NodeId
  , attrTitle :: Text
  , attrType :: Text
  , attrDefault :: Maybe Text
  }
  deriving stock (Eq, Ord, Show)

data AttrClass = ClassNode | ClassEdge
  deriving stock (Eq, Ord, Show)

instance Display AttrClass where
  displayBuilder = \case
    ClassNode -> "node"
    ClassEdge -> "edge"

instance ToXML AttrDef where
  toXML AttrDef {attrId, attrTitle, attrType} =
    elementA
      "attribute"
      [ ("id", display attrId)
      , ("title", display attrTitle)
      , ("type", display attrType)
      ]
      ()

contextAttributeId :: NodeId
contextAttributeId = NodeId "context"

contextAttrDef :: AttrDef
contextAttrDef =
  AttrDef
    { attrId = contextAttributeId
    , attrTitle = "context"
    , attrType = "string"
    , attrDefault = Nothing
    }

contextIdAttrDef :: AttrDef
contextIdAttrDef =
  AttrDef
    { attrId = NodeId "context_id"
    , attrTitle = "Context ID"
    , attrType = "int"
    , attrDefault = Nothing
    }

-- | Graph topology
data Node = Node
  { nodeId :: NodeId
  , nodeLabel :: Label
  , nodeAttrs :: List AttValue
  }
  deriving stock (Eq, Ord, Show)

serviceToGexfNode :: ServiceName -> Maybe ContextName -> Node
serviceToGexfNode serviceName mContextName =
  Node
    { nodeId = NodeId (display serviceName)
    , nodeLabel = Label (display serviceName)
    , nodeAttrs =
        case mContextName of
          Nothing -> []
          Just contextName ->
            [ AttValue
                { forAttrId = contextAttributeId
                , attrValue = display contextName
                }
            , AttValue
                { forAttrId = NodeId "context_id"
                , attrValue = display (100 :: Int)
                }
            ]
    }

entityRefToNode :: EntityName -> Node
entityRefToNode entityName =
  Node
    { nodeId = NodeId (display entityName)
    , nodeLabel = Label (display entityName)
    , nodeAttrs = []
    }

instance ToXML Node where
  toXML Node {nodeId, nodeLabel, nodeAttrs} =
    elementA
      "node"
      [ ("id", display nodeId)
      , ("label", display nodeLabel)
      ]
      $ case nodeAttrs of
        [] -> pure ()
        attrs ->
          element "attvalues" (forM_ attrs toXML)

data Edge = Edge
  { edgeId :: EdgeId
  , edgeSource :: NodeId
  , edgeTarget :: NodeId
  , edgeLabel :: Text
  -- , edgeAttrs :: List Attribute
  }
  deriving stock (Eq, Ord, Show)

instance ToXML Edge where
  toXML Edge {edgeId, edgeSource, edgeTarget, edgeLabel} =
    elementA
      "edge"
      [ ("id", "e" <> display edgeId)
      , ("source", display edgeSource)
      , ("target", display edgeTarget)
      , ("label", display edgeLabel)
      ]
      ()

-- | Attribute values assigned to nodes/edges
data AttValue = AttValue
  { forAttrId :: NodeId
  , attrValue :: Text
  }
  deriving stock (Eq, Ord, Show)

instance ToXML AttValue where
  toXML AttValue {forAttrId, attrValue} =
    elementA
      "attvalue"
      [ ("for", display forAttrId)
      , ("value", display attrValue)
      ]
      ()

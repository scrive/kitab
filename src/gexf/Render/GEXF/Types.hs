module Render.GEXF.Types where

import Text.XML.Writer

import Core.Model.ContextName
import Core.Model.EntityName
import Core.Model.Reference
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
  , attributes :: List Attributes
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

contextIdAttributeId :: NodeId
contextIdAttributeId = NodeId "context_id"

contextIdAttrDef :: AttrDef
contextIdAttrDef =
  AttrDef
    { attrId = contextIdAttributeId
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

-- | Single source of truth for node ids: both node rendering and edge
-- rendering must derive ids through this function, since GEXF edges
-- reference nodes by exact string id and any divergence produces
-- dangling edges. Tool ids are qualified by their owning service.
referenceNodeId :: Reference -> NodeId
referenceNodeId = \case
  ToolRef _ (ServiceName service) toolName -> toolNodeId service toolName
  ref -> NodeId (display ref)

-- | See 'referenceNodeId'.
toolNodeId :: Text -> Text -> NodeId
toolNodeId service toolName = NodeId (service <> "-" <> toolName)

-- | The @context@ / @context_id@ attribute pair shared by service and
-- tool nodes.
contextAttValues :: Text -> List AttValue
contextAttValues context =
  [ AttValue
      { forAttrId = contextAttributeId
      , attrValue = context
      }
  , AttValue
      { forAttrId = contextIdAttributeId
      , attrValue = display (100 :: Int)
      }
  ]

toolToGexfNode :: Text -> Text -> Node
toolToGexfNode service toolName =
  Node
    { nodeId = toolNodeId service toolName
    , nodeLabel = Label toolName
    , nodeAttrs = contextAttValues service
    }

serviceToGexfNode :: ServiceName -> Maybe ContextName -> Node
serviceToGexfNode serviceName mContextName =
  Node
    { nodeId = referenceNodeId (ServiceRef serviceName)
    , nodeLabel = Label (display serviceName)
    , nodeAttrs =
        case mContextName of
          Nothing -> []
          Just contextName -> contextAttValues (display contextName)
    }

entityRefToNode :: EntityName -> Node
entityRefToNode entityName =
  Node
    { nodeId = referenceNodeId (EntityRef entityName)
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

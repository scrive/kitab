module Core.Model.PortNode where

import Data.Word

data PortNode = PortNode
  { port :: Word16
  , protocol :: Text
  }
  deriving stock (Eq, Show, Ord)
  deriving
    (Display)
    via (ShowInstance PortNode)

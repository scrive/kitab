module Core.Model.Reference where

import Core.Model.EntityName
import Core.Model.ServiceName

data Reference
  = ServiceRef ServiceName
  | EntityRef EntityName
  | ToolRef Text
  deriving stock (Eq, Ord, Show)

instance Display Reference where
  displayBuilder = \case
    ServiceRef s -> displayBuilder s
    EntityRef e -> displayBuilder e
    ToolRef t -> displayBuilder t

module Core.Model.Reference where

import Core.Model.ContextName
import Core.Model.EntityName
import Core.Model.Service
import Core.Model.ServiceName

data Reference
  = ServiceRef ServiceName
  | EntityRef EntityName
  | ToolRef (Maybe ContextName) ServiceName Text
  | CIDRRef CIDRConnection
  deriving stock (Eq, Ord, Show)

instance Display Reference where
  displayBuilder = \case
    ServiceRef s -> displayBuilder s
    EntityRef e -> displayBuilder e
    ToolRef _ _ t -> displayBuilder t
    CIDRRef conn -> displayBuilder conn.connectTarget

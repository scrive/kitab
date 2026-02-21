module Core.Model.Reference where

import Core.Model.EntityName
import Core.Model.ServiceName

data Reference
  = ServiceRef ServiceName
  | EntityRef EntityName

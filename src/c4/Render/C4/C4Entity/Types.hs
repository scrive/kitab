module Render.C4.C4Entity.Types where

import Core.Model.EntityName
import Core.Model.Service

data C4Entity = C4Entity
  { entityName :: EntityName
  }

toC4Entity :: EntityAccess -> C4Entity
toC4Entity entity =
  let entityName = entity.accessTarget
  in C4Entity {entityName}

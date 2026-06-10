module Render.Puml.C4Entity.Types where

import Core.Model.EntityName

data C4Entity = C4Entity
  { entityName :: EntityName
  }

toC4Entity :: EntityName -> C4Entity
toC4Entity entityName = C4Entity {entityName}

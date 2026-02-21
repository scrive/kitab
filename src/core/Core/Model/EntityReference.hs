module Core.Model.EntityReference where

import GHC.Generics

import Core.Model.EntityName

data EntityReference = EntityReference
  { referenceName :: EntityName
  }
  deriving stock (Eq, Show, Ord, Generic)

instance Display EntityReference where
  displayBuilder EntityReference {referenceName} = displayBuilder referenceName

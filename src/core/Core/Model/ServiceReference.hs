module Core.Model.ServiceReference where

import GHC.Generics

import Core.Model.ContextName
import Core.Model.ServiceName

data ServiceReference = ServiceReference
  { referenceName :: ServiceName
  , referenceContext :: Maybe ContextName
  }
  deriving stock (Eq, Show, Ord, Generic)

instance Display ServiceReference where
  displayBuilder ServiceReference {referenceName} = displayBuilder referenceName

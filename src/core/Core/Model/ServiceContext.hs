module Core.Model.ServiceContext where

import GHC.Generics

import Core.Model.ContextEntity
import Core.Model.ContextName

data ServiceContext = ServiceContext
  { contextName :: ContextName
  , contextEntities :: List ContextEntity
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Display ServiceContext where
  displayBuilder ServiceContext {contextName} = displayBuilder contextName

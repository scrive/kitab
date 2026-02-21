module Core.Model.ServiceContext where

import GHC.Generics

import Core.Model.ContextName

data ServiceContext = ServiceContext
  { contextName :: ContextName
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Display ServiceContext where
  displayBuilder ServiceContext {contextName} = displayBuilder contextName

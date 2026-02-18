module Core.Model.ServiceContext where

import GHC.Generics

import Core.Model.ContextName
import Core.Model.Service

data ServiceContext = ServiceContext
  { contextName :: ContextName
  , contextServices :: List Service
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Display ServiceContext where
  displayBuilder ServiceContext {contextName} = displayBuilder contextName

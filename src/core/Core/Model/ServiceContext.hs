module Core.Model.ServiceContext where

import Data.String
import GHC.Generics
import Prettyprinter

newtype ContextName = ContextName Text
  deriving newtype (Eq, Ord, Show, IsString, Pretty, Display)

data ServiceContext = ServiceContext
  { contextName :: ContextName
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Display ServiceContext where
  displayBuilder ServiceContext {contextName} = displayBuilder contextName

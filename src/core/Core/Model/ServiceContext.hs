module Core.Model.ServiceContext where

import Data.String
import GHC.Generics
import Prettyprinter

newtype ContextName = ContextName Text
  deriving newtype (Eq, Ord, Show, IsString, Pretty)

data ServiceContext = ServiceContext
  { contextName :: ContextName
  }
  deriving stock (Eq, Ord, Show, Generic)

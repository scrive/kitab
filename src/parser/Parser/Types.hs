module Parser.Types where

import Core.Model.Service
import Core.Model.ServiceContext

data Declaration
  = ServiceDeclaration Service
  | ContextDeclaration ServiceContext
  deriving stock (Eq, Ord, Show)

isService :: Declaration -> Bool
isService (ServiceDeclaration _) = True
isService _ = False

module Parser.Types where

import Core.Model.Entity
import Core.Model.Service
import Core.Model.ServiceContext
import Core.Model.Variable

data Declaration
  = ServiceDeclaration Service
  | ContextDeclaration ServiceContext
  | EntityDeclaration Entity
  | VariableDeclaration Variable
  deriving stock (Eq, Ord, Show)

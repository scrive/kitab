module Parser.V1.Types where

import Core.Model.CIDRSet
import Core.Model.Entity
import Core.Model.Service
import Core.Model.ServiceContext

data Declaration (var :: Type)
  = ServiceDeclaration (Service var)
  | ContextDeclaration ServiceContext
  | EntityDeclaration Entity
  | CIDRSetDeclaration (CIDRSet var)
  deriving stock (Eq, Ord, Show)

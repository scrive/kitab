module Parser.Types where

import Core.Model.Entity
import Core.Model.Inventory
import Core.Model.Service
import Core.Model.ServiceContext

data Declaration
  = ServiceDeclaration Service
  | ContextDeclaration ServiceContext
  | EntityDeclaration Entity
  | InventoryDeclaration Inventory
  deriving stock (Eq, Ord, Show)

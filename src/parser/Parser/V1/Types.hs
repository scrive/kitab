module Parser.V1.Types where

import Core.Model.CIDRSet
import Core.Model.Entity
import Core.Model.Service
import Core.Model.ServiceContext
import Core.Variable

data Declaration (var :: Type)
  = ServiceDeclaration (Service var)
  | ContextDeclaration ServiceContext
  | EntityDeclaration Entity
  | CIDRSetDeclaration (CIDRSet var)
  | VersionDeclaration Word
  | ToolDeclaration Text
  deriving stock (Eq, Ord, Show)

data Declarations = Declarations
  { services :: List (Service Var)
  , entities :: List Entity
  , contexts :: List ServiceContext
  , cidrs :: List (CIDRSet Var)
  }
  deriving stock (Eq, Ord, Show)

-- | Sort a flat list of declarations into their per-kind buckets
partitionDeclarations
  :: List (Declaration Var)
  -> Declarations
partitionDeclarations =
  foldr file (Declarations {services = [], entities = [], contexts = [], cidrs = []})
  where
    file declaration acc = case declaration of
      ServiceDeclaration s -> acc {services = s : acc.services}
      EntityDeclaration e -> acc {entities = e : acc.entities}
      ContextDeclaration c -> acc {contexts = c : acc.contexts}
      CIDRSetDeclaration c -> acc {cidrs = c : acc.cidrs}
      VersionDeclaration _ -> acc
      ToolDeclaration _ -> acc

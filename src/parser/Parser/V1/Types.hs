module Parser.V1.Types where


import Core.Model.CIDRSet
import Core.Model.ContextName
import Core.Model.Entity
import Core.Model.Service
import Core.Model.ServiceContext
import Core.Variable

data Declaration (var :: Type)
  = ServiceDeclaration (Service var)
  | ContextDeclaration ServiceContext
  | EntityDeclaration Entity
  | CIDRSetDeclaration (CIDRSet var)
  | VersionDeclration Word
  | ToolDeclaration Text
  deriving stock (Eq, Ord, Show)

data Declarations = Declarations
  { services :: List (Service Var)
  , entities :: List Entity
  , contexts :: List ContextName
  , cidrs :: List (CIDRSet Var)
  }
  deriving stock (Eq, Ord, Show)

partitionDeclarations
  :: List (Declaration Var)
  -> Declarations
partitionDeclarations declarations =
  let entities =
        mapMaybe
          ( \case
              EntityDeclaration e -> Just e
              _ -> Nothing
          )
          declarations
      contexts =
        mapMaybe
          ( \case
              ContextDeclaration (ServiceContext {contextName}) -> Just contextName
              _ -> Nothing
          )
          declarations
      services =
        declarations
          & mapMaybe
            ( \case
                ServiceDeclaration s -> Just s
                _ -> Nothing
            )

      cidrs =
        mapMaybe
          ( \case
              CIDRSetDeclaration c -> Just c
              _ -> Nothing
          )
          declarations
  in Declarations
       { services
       , entities
       , contexts
       , cidrs
       }

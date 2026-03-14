module Parser.Utils where

data LitVar
  = Literal Text
  | Var Text
  deriving stock (Eq, Ord, Show)

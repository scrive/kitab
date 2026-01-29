module Render.C4.Types where

import Data.Text qualified as T
import Prettyprinter

import Core.Model

newtype C4ServiceAlias = C4ServiceAlias Text
  deriving newtype (Eq, Show, Ord, Pretty)

mkC4ServiceAlias :: ServiceName -> C4ServiceAlias
mkC4ServiceAlias (ServiceName input) =
  input
    & T.replace "-" "_"
    & T.replace " " "_"
    & C4ServiceAlias

data C4Service = C4Service
  { alias :: C4ServiceAlias
  , name :: ServiceName
  }
  deriving stock (Eq, Show, Ord)

toC4Service :: ServiceName -> C4Service
toC4Service serviceName =
  let alias = mkC4ServiceAlias serviceName
      name = serviceName
  in C4Service {alias, name}

{-# LANGUAGE OverloadedLabels #-}

module Render.C4.Types where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Optics.Core
import Prettyprinter

import Core.Model.ContextName
import Core.Model.Service

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
  , systemBoundary :: Maybe ContextName
  }
  deriving stock (Eq, Show, Ord)

toC4Service :: Map ServiceName ServiceInfo -> ServiceName -> C4Service
toC4Service serviceIndex serviceName =
  let alias = mkC4ServiceAlias serviceName
      name = serviceName
      mServiceInfo = Map.lookup serviceName serviceIndex
      systemBoundary = mServiceInfo ^? _Just % #serviceContext % _Just
  in C4Service {alias, name, systemBoundary}

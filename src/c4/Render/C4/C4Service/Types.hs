{-# LANGUAGE OverloadedLabels #-}

module Render.C4.C4Service.Types where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Optics.Core
import Prettyprinter

import Core.Model.ContextName
import Core.Model.EntityName
import Core.Model.Reference
import Core.Model.Service
import Core.Model.ServiceName

newtype C4ServiceAlias = C4ServiceAlias Text
  deriving newtype (Eq, Show, Ord, Pretty)

mkC4ServiceAlias :: Text -> C4ServiceAlias
mkC4ServiceAlias input =
  input
    & T.replace "-" "_"
    & T.replace " " "_"
    & C4ServiceAlias

data C4Service = C4Service
  { alias :: C4ServiceAlias
  , name :: Text
  , boundaryHierarchy :: List ContextName
  }
  deriving stock (Eq, Show, Ord)

toC4Service :: Map ServiceName (ServiceInfo var) -> Reference -> C4Service
toC4Service serviceIndex = \case
  ServiceRef (ServiceName name) ->
    let alias = mkC4ServiceAlias name
        mServiceInfo = Map.lookup (ServiceName name) serviceIndex
        boundaryHierarchy = maybeToList $ mServiceInfo ^? _Just % #serviceContext % _Just
    in C4Service {alias, name, boundaryHierarchy}
  EntityRef (EntityName name) ->
    let alias = mkC4ServiceAlias name
        boundaryHierarchy = []
    in C4Service {alias, name, boundaryHierarchy}
  ToolRef serviceContext (ServiceName serviceName) name ->
    let alias = mkC4ServiceAlias name
        boundaryHierarchy = maybeToList serviceContext <> [ContextName serviceName]
    in C4Service {alias, name, boundaryHierarchy}

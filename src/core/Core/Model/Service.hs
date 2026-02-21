{-# LANGUAGE DerivingVia #-}

module Core.Model.Service where

import Data.Set
import Data.Set qualified as Set
import GHC.Generics
import Prettyprinter

import Core.Model.CIDRSet
import Core.Model.ContextName
import Core.Model.EntityName
import Core.Model.PortNode
import Core.Model.ServiceName

data ConnectionType
  = HTTPS
  | FunctionCall
  deriving stock (Eq, Show, Ord)

instance Pretty ConnectionType where
  pretty = \case
    HTTPS -> "HTTPS"
    FunctionCall -> "Function call"

data Service = Service
  { serviceName :: ServiceName
  , serviceInfo :: ServiceInfo
  , serviceConnections :: List Connection
  , cidrSets :: List CIDRSet
  , entityAccesses :: List EntityAccess
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving
    (Display)
    via (ShowInstance Service)

data ServiceInfo = ServiceInfo
  { serviceFqdn :: Maybe Text
  -- ^ Fqdn is a Cilium thing.
  , serviceContext :: Maybe ContextName
  , servicePorts :: Set PortNode
  }
  deriving stock (Eq, Show, Ord, Generic)

defaultServiceInfo :: ServiceInfo
defaultServiceInfo =
  ServiceInfo
    { serviceFqdn = Nothing
    , serviceContext = Nothing
    , servicePorts = Set.empty
    }

data Connection = Connection
  { connectionWith :: ServiceName
  , connectionType :: ConnectionType
  , connectionPorts :: Set PortNode
  }
  deriving stock (Eq, Show, Ord)

data EntityAccess = EntityAccess
  { accessTarget :: EntityName
  }
  deriving stock (Eq, Show, Ord)

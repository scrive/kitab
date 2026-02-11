{-# LANGUAGE DerivingVia #-}

module Core.Model.Service where

import Data.String (IsString)
import GHC.Generics
import Prettyprinter

import Core.Model.CIDRSet
import Core.Model.ServiceContext

newtype ServiceName = ServiceName Text
  deriving newtype (Eq, Ord, Show, IsString, Pretty, Display)

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
  , connections :: List Connection
  , cidrSets :: List CIDRSet
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving
    (Display)
    via (ShowInstance Service)

data ServiceInfo = ServiceInfo
  { serviceFqdn :: Maybe Text
  -- ^ Fqdn is a Cilium thing.
  , serviceContext :: Maybe ServiceContext
  }
  deriving stock (Eq, Show, Ord, Generic)

defaultServiceInfo :: ServiceInfo
defaultServiceInfo =
  ServiceInfo
    { serviceFqdn = Nothing
    , serviceContext = Nothing
    }

data Connection = Connection
  { connectionWith :: ServiceName
  , connectionType :: ConnectionType
  , connectionPorts :: List PortNode
  }
  deriving stock (Eq, Show, Ord)

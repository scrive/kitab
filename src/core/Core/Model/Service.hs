{-# LANGUAGE DerivingVia #-}

module Core.Model.Service where

import Data.Set
import Data.Set qualified as Set
import GHC.Generics
import Prettyprinter

import Core.Model.ContextName
import Core.Model.EntityName
import Core.Model.PortNode
import Core.Model.ServiceName

data ConnectionType
  = Network
  | FunctionCall
  deriving stock (Eq, Show, Ord)

instance Pretty ConnectionType where
  pretty = \case
    Network -> "Network"
    FunctionCall -> "Function call"

instance Display ConnectionType where
  displayBuilder = \case
    Network -> "Network"
    FunctionCall -> "Function call"

data Service (var :: Type) = Service
  { serviceName :: ServiceName
  , serviceInfo :: ServiceInfo var
  , serviceConnections :: List Connection
  , entityAccesses :: List EntityAccess
  , cidrConnections :: List CIDRConnection
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving
    (Display)
    via (ShowInstance (Service var))

emptyService :: Service var
emptyService =
  Service
    { serviceName = ""
    , serviceInfo = defaultServiceInfo
    , serviceConnections = []
    , entityAccesses = []
    , cidrConnections = []
    }

data ServiceInfo (var :: Type) = ServiceInfo
  { serviceFqdn :: Maybe (Either var Text)
  -- ^ Fqdn is a Cilium thing.
  , serviceContext :: Maybe ContextName
  , servicePorts :: Set PortNode
  }
  deriving stock (Eq, Show, Ord, Generic)

defaultServiceInfo :: ServiceInfo a
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
  , accessPorts :: Set PortNode
  }
  deriving stock (Eq, Show, Ord)

data CIDRConnection = CIDRConnection
  { connectTarget :: Text
  }
  deriving stock (Eq, Show, Ord)

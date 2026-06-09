{-# LANGUAGE DerivingVia #-}

module Core.Model.Service where

import Data.Map.Strict qualified as Map
import Data.Set
import Data.Set qualified as Set
import GHC.Generics
import Prettyprinter

import Core.Model.ContextName
import Core.Model.EntityName
import Core.Model.PortNode
import Core.Model.ServiceName

data ConnectionType
  = HTTPS
  | SMTPS
  | FunctionCall
  | Redis
  | Postgres
  | Domain
  | ExternalTool
  | Browser
  deriving stock (Eq, Show, Ord, Enum, Bounded)

instance Display ConnectionType where
  displayBuilder = \case
    HTTPS -> "https"
    SMTPS -> "smtps"
    FunctionCall -> "function-call"
    Redis -> "redis"
    Postgres -> "postgres"
    Domain -> "domain"
    ExternalTool -> "external-tool"
    Browser -> "browser"

instance Pretty ConnectionType where
  pretty = pretty . display

connectionTypes :: Map Text ConnectionType
connectionTypes =
  [minBound .. maxBound]
    <&> (\connType -> (display connType, connType))
    & Map.fromList

data Service (var :: Type) = Service
  { serviceName :: ServiceName
  , serviceInfo :: ServiceInfo var
  , serviceConnections :: List Connection
  , entityAccesses :: List EntityAccess
  , cidrConnections :: List CIDRConnection
  , toolCalls :: List Text
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
    , toolCalls = []
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

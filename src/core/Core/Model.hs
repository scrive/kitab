module Core.Model where

import Data.String (IsString)
import Prettyprinter

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
  }
  deriving stock (Eq, Show, Ord)

data ServiceInfo = ServiceInfo
  { serviceFqdn :: Maybe Text
  -- ^ Fqdn is a Cilium thing.
  }
  deriving stock (Eq, Show, Ord)

defaultServiceInfo :: ServiceInfo
defaultServiceInfo =
  ServiceInfo
    { serviceFqdn = Nothing
    }

data Connection = Connection
  { connectionWith :: ServiceName
  , connectionType :: ConnectionType
  }
  deriving stock (Eq, Show, Ord)

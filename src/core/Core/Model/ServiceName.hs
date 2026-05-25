module Core.Model.ServiceName where

import Data.String
import Prettyprinter

newtype ServiceName = ServiceName {serviceName :: Text}
  deriving newtype (Eq, Ord, Show, IsString, Pretty, Display)

module Core.Model.ServiceName where

import Data.String
import Prettyprinter

newtype ServiceName = ServiceName Text
  deriving newtype (Eq, Ord, Show, IsString, Pretty, Display)

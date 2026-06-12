module Core.Model.ServiceName where

import Control.DeepSeq
import Data.String
import Prettyprinter

newtype ServiceName = ServiceName Text
  deriving newtype (Eq, Ord, Show, IsString, Pretty, Display, NFData)

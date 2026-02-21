module Core.Model.EntityName where

import Data.String
import Prettyprinter

newtype EntityName = EntityName Text
  deriving newtype (Eq, Ord, Show, IsString, Pretty, Display)

module Core.Model.ContextName where

import Data.String
import Prettyprinter

newtype ContextName = ContextName Text
  deriving newtype (Eq, Ord, Show, IsString, Pretty, Display)

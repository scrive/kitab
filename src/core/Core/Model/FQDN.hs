module Core.Model.FQDN where

import GHC.Generics

data FQDN = FQDN
  { fqdn :: Text
  , props :: Map Text Text
  }
  deriving stock (Eq, Ord, Show, Generic)

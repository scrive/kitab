{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Driver.Environment where

import Effectful
import Effectful.Dispatch.Static
import Effectful.Environment
import Env

data EnvVars = EnvVars
  { noColors :: Bool
  , debug :: Bool
  }
  deriving stock (Eq, Ord, Show)

getEnvironment :: Environment :> es => Eff es EnvVars
getEnvironment =
  unsafeEff_ . Env.parse (header "Environment variables") $
    EnvVars
      <$> switch "NO_COLORS" (help "Disable colours")
      <*> switch "DEBUG" (help "Force verbosity")

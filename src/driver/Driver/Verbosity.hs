module Driver.Verbosity where

data VerbositySetting
  = Verbose
  | Quiet
  deriving stock (Eq, Ord, Show)

isVerbose :: VerbositySetting -> Bool
isVerbose = \case
  Verbose -> True
  Quiet -> False

computeVerbosity
  :: Bool
  -- ^ CLI Option "--quiet"
  -> Bool
  -- ^ Environment variable "DEBUG"
  -> VerbositySetting
computeVerbosity True False = Quiet
computeVerbosity _ True = Verbose
computeVerbosity False False = Verbose

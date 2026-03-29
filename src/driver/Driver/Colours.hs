module Driver.Colours where

import Effectful
import Effectful.FileSystem
import Effectful.FileSystem.IO (hIsTerminalDevice, stdout)
import Layoutz

data TerminalColoursSettings
  = Colours
  | NoColours
  deriving stock (Eq, Ord, Show)

computeTerminalColoursSettings
  :: FileSystem :> es
  => Bool
  -> Eff es TerminalColoursSettings
computeTerminalColoursSettings noColors = do
  weAreInATerminal <- hIsTerminalDevice stdout
  case (weAreInATerminal, noColors) of
    (True, False) -> pure Colours
    (False, _) -> pure NoColours
    (_, True) -> pure NoColours

stylise :: TerminalColoursSettings -> Style -> L -> L
stylise NoColours _ layoutElement = layoutElement
stylise Colours style layoutElement = withStyle style layoutElement

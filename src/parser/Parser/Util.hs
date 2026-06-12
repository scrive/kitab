module Parser.Util
  ( pickAll
  , pickOne
  ) where

import Optics.Core

-- | Collect every focus of the optic across a list of decoded children. Used
-- to project a single constructor out of the heterogeneous @mixedChildren@
-- list each node decoder accumulates.
pickAll :: Is k A_Fold => Optic' k is s a -> List s -> List a
pickAll optic = foldMap (toListOf optic)

-- | The first focus of the optic across a list of decoded children, if any.
pickOne :: Is k A_Fold => Optic' k is s a -> List s -> Maybe a
pickOne optic = listToMaybe . pickAll optic

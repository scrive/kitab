module Render.Cilium.Utils where

import Data.List qualified as List
import Prettyprinter

-- | Helper for "key: value"
keyValue :: Text -> Doc ann -> Doc ann
keyValue k v = pretty k <> ":" <+> v

-- | Helper for "key:" followed by a nested block
keyBlock :: Text -> Doc ann -> Doc ann
keyBlock k v = pretty k <> ":" <> hardline <> v

-- | Helper for a YAML list item: "- " followed by the item,
-- with continuation lines aligned after the dash
listItem :: Doc ann -> Doc ann
listItem d = "-" <+> align d

-- | Helper for "key:" followed by an indented block of rendered items
listBlock :: Text -> (a -> Doc ann) -> List a -> Doc ann
listBlock name f items = keyBlock name . indent 2 $ vsep (List.map f items)

module Parser.Inventory where

import Data.Map.Strict qualified as Map
import KDL
import KDL.Decoder.Internal.Decoder

import Core.Model.Inventory
import Core.Model.InventoryVariable

inventoryDecoder :: DecodeArrow NodeList () Inventory
inventoryDecoder = KDL.nodeWith "inventory" $ do
  name <-
    KDL.optional (KDL.arg @Text)
      >>= \case
        Nothing -> KDL.fail "Expected name for inventory"
        Just n -> pure n
  vars <- do
    results <- KDL.children (KDL.many varDecoder)
    pure $ Map.fromList (fmap (\r -> (r.name, r)) results)
  pure Inventory {name, vars}

varDecoder :: DecodeArrow NodeList () InventoryVariable
varDecoder = KDL.nodeWith "var" $ do
  name <- KDL.argWith variableNameDecoder
  value <- KDL.children $ KDL.argAt "value"
  description <- KDL.children . KDL.optional . KDL.argAt $ "description"
  pure InventoryVariable {name, value, description}

variableNameDecoder :: DecodeArrow Value () VariableName
variableNameDecoder =
  VariableName <$> KDL.text

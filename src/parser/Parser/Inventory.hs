module Parser.Inventory where

import Data.Map.Strict qualified as Map
import KDL

import Core.Model.Inventory
import Core.Model.InventoryVariable

inventoryDecoder :: NodeListDecoder Inventory
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

varDecoder :: NodeListDecoder InventoryVariable
varDecoder = KDL.nodeWith "var" $ do
  name <- KDL.argWith variableNameDecoder
  value <- KDL.children $ KDL.argAt "value"
  description <- KDL.children . KDL.optional . KDL.argAt $ "description"
  pure InventoryVariable {name, value, description}

variableNameDecoder :: ValueDecoder VariableName
variableNameDecoder =
  VariableName <$> KDL.text

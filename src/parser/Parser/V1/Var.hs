module Parser.V1.Var where

import KDL

import Core.Model.InventoryVariable
import Core.Variable

-- | Decode a string argument that is either a literal text or a @(var)@-annotated
-- variable reference.
varOrTextArg :: NodeDecoder (Either Var Text)
varOrTextArg =
  KDL.argWith' ["text", "var"] . KDL.withDecoder KDL.any $
    ( \val -> do
        s <- case val.data_ of
          KDL.String s -> pure s
          _ -> KDL.failM "Expected string"
        case (.identifier.value) <$> val.ann of
          Just "var" -> pure . Left $ Var (VariableName s)
          -- Nothing or Just "text"
          _ -> pure . Right $ s
    )

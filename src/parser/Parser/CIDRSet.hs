module Parser.CIDRSet where

import KDL

import Core.Model.CIDRSet
import Core.Model.InventoryVariable
import Core.Variable
import Parser.PortNode

cidrSetDecoder :: NodeListDecoder (CIDRSet Var)
cidrSetDecoder = KDL.nodeWith "cidr-set" $ do
  setName <- KDL.argWith KDL.text
  ports <- KDL.children $ KDL.many portDecoder
  items <-
    KDL.children $
      KDL.many
        ( cidrDecoder
            <|> exceptionDecoder
        )
  pure $ CIDRSet setName items ports

cidrDecoder :: NodeListDecoder (CIDRSetItem Var)
cidrDecoder = KDL.nodeWith "cidr" $ do
  cidrOrVar <-
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
  case cidrOrVar of
    Left var -> pure $ CIDR (Left var)
    Right cidr -> do
      name <- KDL.argWith KDL.text
      pure $ CIDR (Right (cidr, name))

exceptionDecoder :: NodeListDecoder (CIDRSetItem Var)
exceptionDecoder = KDL.nodeWith "except" $ do
  cidrOrVar <-
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
  case cidrOrVar of
    Left var -> pure $ Except (Left var)
    Right cidr -> do
      reason <- KDL.argWith KDL.text
      pure $ Except (Right (cidr, reason))

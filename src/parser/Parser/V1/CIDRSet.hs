module Parser.V1.CIDRSet where

import Data.List.NonEmpty (NonEmpty ((:|)))
import KDL

import Core.Model.CIDRSet
import Core.Variable
import Parser.V1.PortNode
import Parser.V1.Var

cidrSetDecoder :: NodeListDecoder (CIDRSet Var)
cidrSetDecoder = KDL.nodeWith "cidr-set" $ do
  setName <- KDL.argWith KDL.string
  rendererProps <- KDL.remainingProps
  -- Children of a cidr-set node, in any order. At least one cidr-rule is
  -- required so that the rendered policy always has a destination.
  (cidrRules, ports) <- KDL.children $ do
    firstRule <- cidrRuleDecoder
    mixed <- KDL.many ((Left <$> cidrRuleDecoder) <|> (Right <$> portDecoder))
    let (moreRules, ports) = partitionEithers mixed
    pure (firstRule :| moreRules, ports)
  pure $ CIDRSet {setName, cidrRules, ports, rendererProps}

cidrRuleDecoder :: NodeListDecoder (CidrRuleNode Var)
cidrRuleDecoder = KDL.nodeWith "cidr-rule" $ do
  (cidr, excepts) <- KDL.children $ do
    cidr <- cidrDecoder
    excepts <- KDL.many exceptionDecoder
    pure (cidr, excepts)
  pure $ CidrRuleNode {cidr, excepts}

cidrDecoder :: NodeListDecoder (CidrEntry Var)
cidrDecoder = KDL.nodeWith "cidr" labelledVarOrText

exceptionDecoder :: NodeListDecoder (CidrEntry Var)
exceptionDecoder = KDL.nodeWith "except" labelledVarOrText

-- | A var-or-text argument; a literal text is followed by a label argument
labelledVarOrText :: NodeDecoder (CidrEntry Var)
labelledVarOrText = do
  cidrOrVar <- varOrTextArg
  case cidrOrVar of
    Left var -> pure $ Left var
    Right cidr -> do
      label <- KDL.argWith KDL.string
      pure $ Right (cidr, Just label)

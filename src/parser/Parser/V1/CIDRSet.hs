module Parser.V1.CIDRSet where

import Data.List.NonEmpty (nonEmpty)
import GHC.Generics
import KDL

import Core.Model.CIDRSet
import Core.Model.ContextName
import Core.Model.PortNode (PortNode)
import Core.Variable
import Parser.Util (pickAll, pickOne)
import Parser.V1.PortNode
import Parser.V1.ServiceContext (contextReferenceDecoder)
import Parser.V1.Var

-- | A single child node of a @cidr-set@: a rule, a port, or an @in-context@
-- placement.
data CidrSetChild
  = CidrRuleChild (CidrRuleNode Var)
  | PortChild PortNode
  | ContextChild ContextName
  deriving stock (Eq, Ord, Show, Generic)

cidrSetDecoder :: NodeListDecoder (CIDRSet Var)
cidrSetDecoder = KDL.nodeWith "cidr-set" $ do
  setName <- KDL.argWith KDL.string
  rendererProps <- KDL.remainingProps
  -- Children of a cidr-set node, in any order.
  mixedChildren <-
    KDL.children . KDL.many $
      (CidrRuleChild <$> cidrRuleDecoder)
        <|> (PortChild <$> portDecoder)
        <|> (ContextChild <$> contextReferenceDecoder)
  let ports = pickAll #_PortChild mixedChildren
  let context = pickOne #_ContextChild mixedChildren
  -- At least one cidr-rule is required so the rendered policy always has a destination.
  cidrRules <- case nonEmpty (pickAll #_CidrRuleChild mixedChildren) of
    Just rules -> pure rules
    Nothing -> KDL.fail "A cidr-set requires at least one cidr-rule so the rendered policy always has a destination."
  pure $ CIDRSet {setName, cidrRules, ports, context, rendererProps}

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

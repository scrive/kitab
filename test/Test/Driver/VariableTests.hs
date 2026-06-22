module Test.Driver.VariableTests (test) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Test.Tasty

import CLI.Error
import Core.Model.CIDRSet
import Core.Model.Inventory.Aggregated
import Core.Model.InventoryVariable
import Core.Variable
import Driver.Variable
import Test.Utils

test :: TestTree
test =
  testGroup
    "Driver.Variable"
    [ testGroup
        "resolveCIDRVar"
        [ testThat "Literal CIDR passes through unchanged" testLiteralCidrUnchanged
        , testThat "Variable CIDR resolves to value and description" testVariableCidrResolved
        ]
    , testGroup
        "resolveCIDRVars"
        [ testThat "Variables in cidr and excepts are resolved" testCidrSetVarsResolved
        , testThat "Unresolved variable is reported" testUnresolvedCidrVar
        ]
    ]

inventory :: AggregatedInventory
inventory =
  AggregatedInventory
    { aggregatedAttributes = Map.empty
    , aggregatedVars =
        Map.fromList
          [("cidr-var", InventoryVariable {name = "cidr-var", value = "10.0.0.0/8", description = Just "internal"})]
    }

-- | A single-rule CIDR set; only the rule varies between these tests.
mkCidrSet :: CidrRuleNode var -> CIDRSet var
mkCidrSet rule =
  CIDRSet
    { setName = "cs"
    , cidrRules = NE.singleton rule
    , ports = []
    , context = Nothing
    , rendererProps = Map.empty
    }

testLiteralCidrUnchanged :: TestEff Unit
testLiteralCidrUnchanged = do
  result <- resolveCIDRVar inventory (Right ("192.168.0.0/16", Just "lan"))
  assertEqual
    "Literal CIDR entry is preserved"
    (Right ("192.168.0.0/16", Just "lan"))
    result

testVariableCidrResolved :: TestEff Unit
testVariableCidrResolved = do
  result <- resolveCIDRVar inventory (Left (Var "cidr-var"))
  assertEqual
    "Variable resolves to its value and description"
    (Right ("10.0.0.0/8", Just "internal"))
    result

testCidrSetVarsResolved :: TestEff Unit
testCidrSetVarsResolved = do
  let cidrSet =
        mkCidrSet
          CidrRuleNode
            { cidr = Left (Var "cidr-var")
            , excepts = [Left (Var "cidr-var"), Right ("172.16.0.0/12", Nothing)]
            }
  let expected =
        mkCidrSet
          CidrRuleNode
            { cidr = Right ("10.0.0.0/8", Just "internal")
            , excepts = [Right ("10.0.0.0/8", Just "internal"), Right ("172.16.0.0/12", Nothing)]
            }
  resolved <- resolveCIDRVars inventory cidrSet
  assertEqual "Both cidr and excepts variables are resolved" expected resolved

testUnresolvedCidrVar :: TestEff Unit
testUnresolvedCidrVar = do
  let cidrSet = mkCidrSet CidrRuleNode {cidr = Left (Var "missing"), excepts = []}
  result <- runCapturingErrors (resolveCIDRVars inventory cidrSet)
  errors <- assertLeft "Unresolved CIDR variable should fail" result
  assertEqual
    "The missing variable is reported"
    (NE.singleton (variableNotFound "missing"))
    errors

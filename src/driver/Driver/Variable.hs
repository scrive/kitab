module Driver.Variable where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Void
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error

import CLI.Error
import Core.Model.CIDRSet
import Core.Model.Inventory.Aggregated
import Core.Model.Inventory.Aggregated qualified as Inventory
import Core.Model.InventoryVariable
import Core.Model.Service
import Core.Variable (Var (..))

-- | Look a variable up in the aggregated inventory, throwing a 'CLIError'
-- when it is undefined.
resolveVar
  :: Error (NonEmpty CLIError) :> es
  => AggregatedInventory
  -> VariableName
  -> Eff es InventoryVariable
resolveVar inventory variable =
  case Inventory.lookup inventory variable of
    Just result -> pure result
    Nothing -> Error.throwError (NE.singleton $ variableNotFound variable)

resolveServiceVars
  :: Error (NonEmpty CLIError) :> es
  => AggregatedInventory
  -> Service Var
  -> Eff es (Service Void)
resolveServiceVars inventory service = do
  let serviceInfo = service.serviceInfo
  resolvedFqdn <- forM serviceInfo.serviceFqdn $ \case
    Left (Var variable) -> Right . (.value) <$> resolveVar inventory variable
    Right value -> pure (Right value)
  pure $ service {serviceInfo = serviceInfo {serviceFqdn = resolvedFqdn}}

resolveCIDRVars
  :: Error (NonEmpty CLIError) :> es
  => AggregatedInventory
  -> CIDRSet Var
  -> Eff es (CIDRSet Void)
resolveCIDRVars inventory cidrSet = do
  resolvedRules <- traverse (resolveCidrRuleNodeVars inventory) cidrSet.cidrRules
  pure $ cidrSet {cidrRules = resolvedRules}

resolveCidrRuleNodeVars
  :: Error (NonEmpty CLIError) :> es
  => AggregatedInventory
  -> CidrRuleNode Var
  -> Eff es (CidrRuleNode Void)
resolveCidrRuleNodeVars inventory ruleNode = do
  resolvedCidr <- resolveCIDRVar inventory ruleNode.cidr
  resolvedExcepts <- traverse (resolveCIDRVar inventory) ruleNode.excepts
  pure $ ruleNode {cidr = resolvedCidr, excepts = resolvedExcepts}

resolveCIDRVar
  :: Error (NonEmpty CLIError) :> es
  => AggregatedInventory
  -> CidrEntry Var
  -> Eff es (CidrEntry Void)
resolveCIDRVar inventory cidrVar = case cidrVar of
  Right value -> pure $ Right value
  Left (Var variable) -> do
    result <- resolveVar inventory variable
    pure (Right (result.value, result.description))

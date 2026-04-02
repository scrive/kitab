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

resolveServiceInfoVars
  :: Error (NonEmpty CLIError) :> es
  => AggregatedInventory
  -> ServiceInfo Var
  -> Eff es (ServiceInfo Void)
resolveServiceInfoVars inventory si@ServiceInfo {serviceFqdn} = do
  case serviceFqdn of
    Just (Left (Var variable)) -> do
      case Inventory.lookup inventory variable of
        Just result ->
          pure $ si {serviceFqdn = Just (Right result.value)}
        Nothing -> Error.throwError (NE.singleton $ variableNotFound variable)
    Just (Right value) ->
      pure $
        si {serviceFqdn = Just (Right value)}
    Nothing -> pure si {serviceFqdn = Nothing}

resolveServiceVars
  :: Error (NonEmpty CLIError) :> es
  => AggregatedInventory
  -> Service Var
  -> Eff es (Service Void)
resolveServiceVars inventory service = do
  resolvedServiceInfo <- resolveServiceInfoVars inventory service.serviceInfo
  pure $ service {serviceInfo = resolvedServiceInfo}

resolveCIDRVars
  :: Error (NonEmpty CLIError) :> es
  => AggregatedInventory
  -> CIDRSet Var
  -> Eff es (CIDRSet Void)
resolveCIDRVars inventory cidrSet = do
  resolvedCIDRSetItems <- traverse (resolveCIDRItemVar inventory) cidrSet.items
  pure $ cidrSet {items = resolvedCIDRSetItems}

resolveCIDRItemVar
  :: Error (NonEmpty CLIError) :> es
  => AggregatedInventory
  -> CIDRSetItem Var
  -> Eff es (CIDRSetItem Void)
resolveCIDRItemVar inventory item =
  case item of
    CIDR (Right value) -> pure $ CIDR (Right value)
    Except (Right value) -> pure $ Except (Right value)
    CIDR (Left (Var variable)) ->
      case Inventory.lookup inventory variable of
        Just result -> pure (CIDR (Right (result.value, fromMaybe "" result.description)))
        Nothing -> Error.throwError (NE.singleton $ variableNotFound variable)
    Except (Left (Var variable)) ->
      case Inventory.lookup inventory variable of
        Just result -> pure (CIDR (Right (result.value, fromMaybe "" result.description)))
        Nothing -> Error.throwError (NE.singleton $ variableNotFound variable)

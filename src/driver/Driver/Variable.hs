module Driver.Variable where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Void
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error

import CLI.Error
import Core.Model.Inventory.Aggregated
import Core.Model.Inventory.Aggregated qualified as Inventory
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
          pure $ si {serviceFqdn = Just (Right result)}
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

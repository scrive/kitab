module Driver.Variable where

import Data.Void
import Effectful

import Core.Model.Inventory
import Core.Model.Service
import Core.Variable (Var)
import Core.Variable qualified as Variable

resolveServiceInfoVars
  :: AggregatedInventory
  -> ServiceInfo Var
  -> Eff es (ServiceInfo Void)
resolveServiceInfoVars inventory si@ServiceInfo {serviceFqdn} = do
  case serviceFqdn of
    Just (Left variable) -> do
      result <- Variable.lookup inventory variable
      pure $ si {serviceFqdn = Just (Right result)}
    Just (Right value) ->
      pure $
        si {serviceFqdn = Just (Right value)}
    Nothing -> pure si {serviceFqdn = Nothing}

resolveServiceVars
  :: AggregatedInventory
  -> Service Var
  -> Eff es (Service Void)
resolveServiceVars inventory service = do
  resolvedServiceInfo <- resolveServiceInfoVars inventory service.serviceInfo
  pure $ service {serviceInfo = resolvedServiceInfo}

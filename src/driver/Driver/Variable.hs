module Driver.Variable where

import Data.Void
import Effectful
import Effectful.Reader.Static (Reader)

import Core.Model.Inventory
import Core.Variable (Var)
import Core.Variable qualified as Variable
import Parser.Service

resolveServiceMetadataVars
  :: Reader AggregatedInventory :> es
  => ServiceMetadata Var
  -> Eff es (ServiceMetadata Void)
resolveServiceMetadataVars (FQDNNode (Right value)) = pure $ FQDNNode (Right value)
resolveServiceMetadataVars (FQDNNode (Left variable)) = do
  result <- Variable.lookup variable
  pure $ FQDNNode (Right result)
resolveServiceMetadataVars e = pure e

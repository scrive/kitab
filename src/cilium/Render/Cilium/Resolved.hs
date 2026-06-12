-- | Resolution of services against the declaration indices.
--
-- 'resolveService' proves that every egress target of a service is declared
-- and reachable, producing a 'ResolvedService' that
-- 'Render.Cilium.toCiliumPolicy' renders without failure. All violations are
-- accumulated and reported together.
module Render.Cilium.Resolved
  ( Route (..)
  , ResolvedConnection (..)
  , ResolvedEntityAccess (..)
  , ResolvedService (..)
  , ResolutionError (..)
  , resolveService
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void
import Validation

import Core.Model.CIDRSet
import Core.Model.ContextName
import Core.Model.Entity
import Core.Model.EntityName
import Core.Model.PortNode
import Core.Model.Service
import Core.Model.ServiceName

-- | How a service reaches a connection target in a Cilium policy.
--
-- A target that is neither in the same context nor exposed through an fqdn
-- has no 'Route': 'resolveService' rejects it, because an egress rule with
-- no destination would allow its ports towards /all/ destinations.
data Route
  = SameContext
  | ViaFqdn Text
  deriving stock (Eq, Show)

-- | A connection whose target has been proven declared and reachable.
data ResolvedConnection = ResolvedConnection
  { connectionTarget :: ServiceName
  , connectionRoute :: Route
  , connectionPorts :: Set PortNode
  -- ^ Already picked per the port selection rules of 'pickServicePorts'
  }
  deriving stock (Eq, Show)

-- | An entity access whose target has been proven declared.
data ResolvedEntityAccess = ResolvedEntityAccess
  { accessTarget :: EntityName
  , accessPorts :: Set PortNode
  -- ^ Already picked per the port selection rules of 'pickEntityPorts'
  }
  deriving stock (Eq, Show)

-- | A service whose egress targets have all been resolved against the
-- declaration indices. Rendering cannot fail on it.
data ResolvedService = ResolvedService
  { serviceName :: ServiceName
  , serviceContext :: Maybe ContextName
  , cidrTargets :: List (CIDRSet Void)
  , serviceTargets :: List ResolvedConnection
  , entityTargets :: List ResolvedEntityAccess
  }
  deriving stock (Eq, Show)

-- | A reference that cannot be rendered as a Cilium egress rule.
-- The first field is always the source service.
data ResolutionError
  = MissingService ServiceName ServiceName
  | UnreachableService ServiceName ServiceName
  | MissingCidrSet ServiceName Text
  | MissingEntity ServiceName EntityName
  deriving stock (Eq, Show)

-- | Resolve every egress target of a service: service targets must be
-- declared and reachable (per 'routeTo'), cidr-set and entity targets must
-- be declared.
resolveService
  :: Map ServiceName (ServiceInfo Void)
  -> Map EntityName EntityInfo
  -> Map Text (CIDRSet Void)
  -> Service Void
  -> Validation (NonEmpty ResolutionError) ResolvedService
resolveService serviceIndex entityIndex cidrIndex service =
  ResolvedService service.serviceName service.serviceInfo.serviceContext
    <$> traverse resolveCidrConnection service.cidrConnections
    <*> traverse resolveConnection service.serviceConnections
    <*> traverse resolveEntityAccess service.entityAccesses
  where
    source = service.serviceName

    resolveCidrConnection :: CIDRConnection -> Validation (NonEmpty ResolutionError) (CIDRSet Void)
    resolveCidrConnection CIDRConnection {connectTarget} =
      maybeToSuccess
        (pure $ MissingCidrSet source connectTarget)
        (Map.lookup connectTarget cidrIndex)

    resolveConnection :: Connection -> Validation (NonEmpty ResolutionError) ResolvedConnection
    resolveConnection Connection {connectionWith, connectionPorts} =
      case Map.lookup connectionWith serviceIndex of
        Nothing -> failure $ MissingService source connectionWith
        Just serviceInfo ->
          case routeTo service.serviceInfo.serviceContext serviceInfo of
            Nothing -> failure $ UnreachableService source connectionWith
            Just route ->
              Success
                ResolvedConnection
                  { connectionTarget = connectionWith
                  , connectionRoute = route
                  , connectionPorts = pickServicePorts serviceInfo connectionPorts
                  }

    resolveEntityAccess :: EntityAccess -> Validation (NonEmpty ResolutionError) ResolvedEntityAccess
    resolveEntityAccess EntityAccess {accessTarget, accessPorts} =
      case Map.lookup accessTarget entityIndex of
        Nothing -> failure $ MissingEntity source accessTarget
        Just entityInfo ->
          Success
            ResolvedEntityAccess
              { accessTarget
              , accessPorts = pickEntityPorts entityInfo accessPorts
              }

-- | The 'Route' from a service (identified by its context) to a connection
-- target, if any. This is the single source of truth for the reachability
-- rule.
--
-- Reachability is exact-context equality: each context is its own cluster.
-- Context nesting ('Core.Model.ServiceContext.subContexts') is a PlantUML
-- grouping concern and is deliberately invisible here — a parent and a nested
-- sub-context are distinct clusters, so a cross-context target still needs an
-- fqdn.
routeTo :: Maybe ContextName -> ServiceInfo Void -> Maybe Route
routeTo mContext ServiceInfo {serviceContext, serviceFqdn}
  | isJust serviceContext
  , serviceContext == mContext =
      Just SameContext
  | Just (Right hostname) <- serviceFqdn = Just (ViaFqdn hostname)
  | otherwise = Nothing

-- | Select which ports to use for a target:
-- 1. If no ports are specified by the caller, use the ones opened by the target
-- 2. If the caller specifies ports and they are a subset of the target's ports, use them
-- 3. Otherwise use the fallback
pickPorts
  :: Set PortNode
  -- ^ Fallback ports
  -> Set PortNode
  -- ^ Target ports
  -> Set PortNode
  -- ^ Caller ports
  -> Set PortNode
pickPorts fallback targetPorts ports
  | Set.null ports = targetPorts
  | ports `Set.isSubsetOf` targetPorts = ports
  | otherwise = fallback

-- | 'pickPorts' against a service's ports, falling back to port 443/TCP
pickServicePorts :: ServiceInfo Void -> Set PortNode -> Set PortNode
pickServicePorts ServiceInfo {servicePorts} =
  pickPorts (Set.singleton (PortNode 443 "TCP")) servicePorts

-- | 'pickPorts' against an entity's ports, falling back to no ports at all
pickEntityPorts :: EntityInfo -> Set PortNode -> Set PortNode
pickEntityPorts EntityInfo {entityPorts} =
  pickPorts Set.empty entityPorts

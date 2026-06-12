module Core.Model.ServiceContext where

import Data.Map.Strict qualified as Map
import GHC.Generics

import Core.Model.ContextName

data ServiceContext = ServiceContext
  { contextName :: ContextName
  , subContexts :: List ServiceContext
  -- ^ Nested contexts. These are a PlantUML grouping concern only: they make a
  -- context render inside its parent's boundary. They carry no Cilium meaning —
  -- each context is a separate cluster, so a parent and a nested sub-context are
  -- never the "same context" for reachability ('Render.Cilium.Resolved.routeTo').
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Display ServiceContext where
  displayBuilder ServiceContext {contextName} = displayBuilder contextName

-- | Every declared context paired with its full hierarchy (the path of context
-- names from the root down to, and including, the context itself), in
-- declaration order. 'contextHierarchies' is a projection of this single walk.
--
-- For @context "company" { context "k8s" }@ this yields
-- @[("company", ["company"]), ("k8s", ["company", "k8s"])]@.
contextPaths :: List ServiceContext -> List (Tuple2 ContextName (List ContextName))
contextPaths = go []
  where
    go prefix =
      concatMap
        ( \ServiceContext {contextName, subContexts} ->
            let path = prefix <> [contextName]
            in (contextName, path) : go path subContexts
        )

-- | Map every declared context to its full hierarchy. On a name declared with
-- more than one path the last declaration wins; callers that must reject such
-- ambiguity should inspect 'contextPaths' directly.
contextHierarchies :: List ServiceContext -> Map ContextName (List ContextName)
contextHierarchies = Map.fromList . contextPaths

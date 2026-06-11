{-# LANGUAGE QuasiQuotes #-}

module Driver.Puml where

import Algebra.Graph.Labelled (Graph)
import Algebra.Graph.Labelled qualified as Graph
import Algebra.Graph.Labelled.AdjacencyMap qualified as AM
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem (FileSystem)
import System.OsPath (OsPath, osp, (</>))
import System.OsPath qualified as OsPath
import Validation

import CLI.Error
import Core.Model.CIDRSet
import Core.Model.Reference
import Core.Model.Service
import Core.Model.ServiceName
import Driver.Output (writeArtifact)
import Driver.Verbosity
import Render.Puml qualified as Puml
import Render.Puml.C4Container.Types qualified as Puml

renderToPuml
  :: (Console :> es, Error (NonEmpty CLIError) :> es, FileSystem :> es)
  => Map ServiceName (ServiceInfo var)
  -> Map Text (CIDRSet var)
  -> OsPath
  -> VerbositySetting
  -> Graph (List ConnectionType) Reference
  -> Eff es Unit
renderToPuml serviceIndex cidrIndex outputDir verbosity graph = do
  let edges = Graph.edgeList graph
  containersByRef <- validateContainers serviceIndex cidrIndex edges
  let graphEdges =
        edges
          & fmap (\(es, a, b) -> (es, containersByRef Map.! a, containersByRef Map.! b))
  let adjacencyMap = AM.edges graphEdges
  let rendered = Puml.renderPuml adjacencyMap
  outputPath <- OsPath.decodeUtf (outputDir </> [osp|architecture.puml|])
  writeArtifact verbosity outputPath rendered

validateContainers
  :: Error (NonEmpty CLIError) :> es
  => Map ServiceName (ServiceInfo var)
  -> Map Text (CIDRSet var)
  -> List (Tuple3 (List ConnectionType) Reference Reference)
  -> Eff es (Map Reference Puml.C4Container)
validateContainers serviceIndex cidrIndex edges =
  case traverse validate references of
    Failure errors -> Error.throwError $ fmap pumlValidationError errors
    Success pairs -> pure (Map.fromList pairs)
  where
    edgeReferences = concatMap (\(_, a, b) -> [a, b]) edges
    serviceReferences = ServiceRef <$> Map.keys serviceIndex
    references = Set.toList . Set.fromList $ edgeReferences <> serviceReferences
    validate reference =
      case Puml.toC4Container serviceIndex cidrIndex reference of
        Left err -> Failure (NE.singleton err)
        Right container -> Success (reference, container)

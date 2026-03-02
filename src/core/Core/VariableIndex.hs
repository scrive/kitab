module Core.VariableIndex where

import Data.List qualified as List
import Data.Map.Strict qualified as Map

import Core.Model.Variable

buildVariableIndex :: List Variable -> Map Text Variable
buildVariableIndex = List.foldl' (\acc var -> Map.insert var.varName var acc) Map.empty

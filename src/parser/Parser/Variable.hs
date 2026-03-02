module Parser.Variable
  ( variableDecoder
  ) where

import Data.Set
import Data.Set qualified as Set
import KDL
import KDL.Decoder.Internal.Decoder

import Core.Model.Variable

variableDecoder :: DecodeArrow NodeList () Variable
variableDecoder = KDL.nodeWith "variable" $ do
  varName <- KDL.argWith KDL.text
  KDL.children $ do
    varType <- varTypeDecoder
    varDescription <- varDescriptionDecoder
    varValues <- optional varValuesDecoder
    if varValues == Just mempty
      then KDL.fail $ "Field \"values\" of variable \"" <> varName <> "\" cannot be empty"
      else
        pure
          Variable {varName, varType, varDescription, varValues}

varTypeDecoder :: DecodeArrow NodeList () Text
varTypeDecoder = KDL.nodeWith "type" $ do
  KDL.argWith KDL.text

varDescriptionDecoder :: DecodeArrow NodeList () Text
varDescriptionDecoder = KDL.nodeWith "description" $ do
  KDL.argWith KDL.text

varValuesDecoder :: DecodeArrow NodeList () (Set Text)
varValuesDecoder =
  KDL.nodeWith "values" $
    Set.fromList <$> KDL.many KDL.arg

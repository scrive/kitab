module Parser.ContextEntity where

import Data.Set qualified as Set
import KDL
import KDL.Decoder.Internal.Decoder

import Core.Model.ContextEntity
import Parser.EntityName
import Parser.PortNode

entityDecoder :: DecodeArrow NodeList () ContextEntity
entityDecoder = KDL.nodeWith "entity" $ do
  entityName <- KDL.argWith entityNameDecoder
  ports <- KDL.children . KDL.many $ portDecoder
  let entityInfo = EntityInfo (Set.fromList ports)
  pure ContextEntity {entityName, entityInfo}

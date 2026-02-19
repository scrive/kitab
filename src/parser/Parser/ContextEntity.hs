module Parser.ContextEntity where

import Data.Set qualified as Set
import KDL
import KDL.Decoder.Internal.Decoder

import Core.Model.ContextEntity
import Parser.PortNode
import Parser.ServiceName

entityDecoder :: DecodeArrow NodeList () ContextEntity
entityDecoder = KDL.nodeWith "entity" $ do
  entityName <- KDL.argWith serviceNameDecoder
  ports <- KDL.children . KDL.many $ portDecoder
  let entityInfo = EntityInfo (Set.fromList ports)
  pure ContextEntity {entityName, entityInfo}

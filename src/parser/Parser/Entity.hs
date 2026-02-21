module Parser.Entity where

import Data.Set qualified as Set
import KDL
import KDL.Decoder.Internal.Decoder

import Core.Model.Entity
import Parser.PortNode
import Parser.ServiceName

entityDecoder :: DecodeArrow NodeList () Entity
entityDecoder = KDL.nodeWith "entity" $ do
  entityName <- KDL.argWith serviceNameDecoder
  ports <- KDL.children . KDL.many $ portDecoder
  let entityInfo = EntityInfo (Set.fromList ports)
  pure Entity {entityName, entityInfo}

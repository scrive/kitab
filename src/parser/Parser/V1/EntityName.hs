module Parser.V1.EntityName where

import KDL

import Core.Model.EntityName

entityNameDecoder :: ValueDecodeArrow Unit EntityName
entityNameDecoder = EntityName <$> KDL.string

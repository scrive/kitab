module Parser.EntityName where

import KDL

import Core.Model.EntityName

entityNameDecoder :: ValueDecodeArrow () EntityName
entityNameDecoder = EntityName <$> KDL.text

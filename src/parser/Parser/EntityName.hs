module Parser.EntityName where

import KDL

import Core.Model.ContextEntity

entityNameDecoder :: ValueDecodeArrow () EntityName
entityNameDecoder = EntityName <$> KDL.text

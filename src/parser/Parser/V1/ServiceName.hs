module Parser.V1.ServiceName where

import KDL

import Core.Model.ServiceName

serviceNameDecoder :: ValueDecoder ServiceName
serviceNameDecoder = ServiceName <$> KDL.string

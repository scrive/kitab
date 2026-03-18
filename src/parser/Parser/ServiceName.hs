module Parser.ServiceName where

import KDL

import Core.Model.ServiceName

serviceNameDecoder :: ValueDecoder ServiceName
serviceNameDecoder = ServiceName <$> KDL.text

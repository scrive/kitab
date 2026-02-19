module Parser.ServiceName where

import KDL

import Core.Model.Service

serviceNameDecoder :: ValueDecodeArrow () ServiceName
serviceNameDecoder = ServiceName <$> KDL.text

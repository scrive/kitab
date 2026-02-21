module Parser.ServiceName where

import KDL

import Core.Model.ServiceName

serviceNameDecoder :: ValueDecodeArrow () ServiceName
serviceNameDecoder = ServiceName <$> KDL.text

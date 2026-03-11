module Parser.ServiceName where

import KDL
import KDL.Decoder.Internal.Decoder

import Core.Model.ServiceName

serviceNameDecoder :: DecodeArrow Value () ServiceName
serviceNameDecoder = ServiceName <$> KDL.text

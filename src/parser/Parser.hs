module Parser where

import KDL

import Parser.Service
import Parser.ServiceContext
import Parser.Types

decodeServiceDocument :: KDL.DocumentDecoder [Declaration]
decodeServiceDocument =
  KDL.document . KDL.many $
    oneOf
      [ KDL.nodeWith
          "context"
          (ContextDeclaration <$> contextDecoder)
      , KDL.nodeWith
          "service"
          (ServiceDeclaration <$> serviceDecoder)
      ]

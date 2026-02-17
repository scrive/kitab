module Parser where

import KDL

import Parser.Service
import Parser.ServiceContext
import Parser.Types

decodeServiceDocument :: KDL.DocumentDecoder [Declaration]
decodeServiceDocument =
  KDL.document . KDL.many $
    oneOf
      [ ContextDeclaration <$> contextDecoder
      , ServiceDeclaration <$> serviceDecoder
      ]

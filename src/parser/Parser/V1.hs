module Parser.V1 where

import KDL

import Core.Variable
import Parser.V1.CIDRSet
import Parser.V1.Entity
import Parser.V1.Service
import Parser.V1.ServiceContext
import Parser.V1.Types

decodeServiceDocument :: KDL.DocumentDecoder [Declaration Var]
decodeServiceDocument =
  KDL.document . KDL.many $
    oneOf
      [ EntityDeclaration <$> entityDecoder
      , CIDRSetDeclaration <$> cidrSetDecoder
      , ContextDeclaration <$> contextDecoder
      , ServiceDeclaration <$> serviceDecoder
      ]

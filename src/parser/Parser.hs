module Parser where

import KDL

import Core.Variable
import Parser.CIDRSet
import Parser.Entity
import Parser.Service
import Parser.ServiceContext
import Parser.Types

decodeServiceDocument :: KDL.DocumentDecoder [Declaration Var]
decodeServiceDocument =
  KDL.document . KDL.many $
    oneOf
      [ EntityDeclaration <$> entityDecoder
      , CIDRSetDeclaration <$> cidrSetDecoder
      , ContextDeclaration <$> contextDecoder
      , ServiceDeclaration <$> serviceDecoder
      ]

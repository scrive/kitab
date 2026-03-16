module Parser where

import KDL

import Core.Model.InventoryVariable
import Parser.Entity
import Parser.Service
import Parser.ServiceContext
import Parser.Types

decodeServiceDocument :: KDL.DocumentDecoder [Declaration VariableName]
decodeServiceDocument =
  KDL.document . KDL.many $
    oneOf
      [ EntityDeclaration <$> entityDecoder
      , ContextDeclaration <$> contextDecoder
      , ServiceDeclaration <$> serviceDecoder
      ]

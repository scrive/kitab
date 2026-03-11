module Parser where

import KDL

import Parser.Entity
import Parser.Inventory
import Parser.Service
import Parser.ServiceContext
import Parser.Types

decodeServiceDocument :: KDL.DocumentDecoder [Declaration]
decodeServiceDocument =
  KDL.document . KDL.many $
    oneOf
      [ EntityDeclaration <$> entityDecoder
      , ContextDeclaration <$> contextDecoder
      , ServiceDeclaration <$> serviceDecoder
      , InventoryDeclaration <$> inventoryDecoder
      ]

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
      [ VersionDeclration <$> versionDecoder
      , EntityDeclaration <$> entityDecoder
      , CIDRSetDeclaration <$> cidrSetDecoder
      , ContextDeclaration <$> contextDecoder
      , ToolDeclaration <$> toolDeclarationDecoder
      , ServiceDeclaration <$> serviceDecoder
      ]

versionDecoder :: NodeListDecoder Word
versionDecoder = KDL.nodeWith "version" $ do
  KDL.arg @Word

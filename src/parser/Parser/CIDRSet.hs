module Parser.CIDRSet where

import KDL

import Core.Model.CIDRSet
import Parser.PortNode

cidrSetDecoder :: NodeListDecoder CIDRSet
cidrSetDecoder = KDL.nodeWith "cidr-set" $ do
  setName <- KDL.argWith KDL.text
  ports <- KDL.children $ KDL.many portDecoder
  items <-
    KDL.children $
      KDL.many
        ( cidrDecoder
            <|> exceptionDecoder
        )
  pure $ CIDRSet setName items ports

cidrDecoder :: NodeListDecoder CIDRSetItem
cidrDecoder = KDL.nodeWith "cidr" $ do
  cidr <- KDL.arg @Text
  name <- KDL.arg @Text
  pure $ CIDR cidr name

exceptionDecoder :: NodeListDecoder CIDRSetItem
exceptionDecoder = KDL.nodeWith "except" $ do
  cidr <- KDL.arg @Text
  reason <- KDL.arg @Text
  pure $ Except cidr reason

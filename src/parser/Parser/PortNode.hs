module Parser.PortNode where

import KDL
import KDL.Decoder.Internal.Decoder

import Core.Model.PortNode

portDecoder :: DecodeArrow NodeList () PortNode
portDecoder = KDL.nodeWith "port" $ do
  port <- KDL.arg
  protocol <- KDL.option "TCP" KDL.arg
  pure PortNode {port, protocol}

module Parser.V1.PortNode where

import KDL

import Core.Model.PortNode

portDecoder :: NodeListDecoder PortNode
portDecoder = KDL.nodeWith "port" $ do
  port <- KDL.arg
  protocol <- KDL.option "TCP" KDL.arg
  pure PortNode {port, protocol}

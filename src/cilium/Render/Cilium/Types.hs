module Render.Cilium.Types
  ( FQDNSelector (..)
  , prettyFQDNSelector
  , L7Rules (..)
  , prettyL7Rules
  , PortRule (..)
  , prettyPortRule
  , PortProtocol (..)
  , prettyPortProtocol
  , fromPortNode
  ) where

import Prettyprinter

import Core.Model.PortNode
import Render.Cilium.Utils

-- | Matches Cilium's FQDNSelector (used in toFQDNs and in L7 DNS rules)
data FQDNSelector
  = MatchName Text
  | MatchPattern Text
  deriving stock (Show, Eq, Ord)

prettyFQDNSelector :: FQDNSelector -> Doc ann
prettyFQDNSelector = \case
  MatchName name -> keyValue "matchName" (dquotes $ pretty name)
  MatchPattern pat -> keyValue "matchPattern" (dquotes $ pretty pat)

-- | Matches Cilium's L7Rules struct
data L7Rules = L7Rules
  { dns :: List FQDNSelector
  }
  deriving stock (Show, Eq, Ord)

prettyL7Rules :: L7Rules -> Doc ann
prettyL7Rules L7Rules {dns} =
  keyBlock "rules" . indent 2 $ listBlock "dns" (listItem . prettyFQDNSelector) dns

-- | Matches Cilium's PortRule struct
data PortRule = PortRule
  { ports :: List PortProtocol
  , rules :: Maybe L7Rules
  }
  deriving stock (Show, Eq, Ord)

prettyPortRule :: PortRule -> Doc ann
prettyPortRule PortRule {ports, rules} =
  listItem . vsep $
    [listBlock "ports" prettyPortProtocol ports]
      <> maybe [] (\r -> [prettyL7Rules r]) rules

-- | Specific port/protocol pair
data PortProtocol = PortProtocol
  { port :: Text
  , protocol :: Text
  }
  deriving stock (Show, Eq, Ord)

fromPortNode :: PortNode -> PortProtocol
fromPortNode portNode = PortProtocol (display portNode.port) portNode.protocol

prettyPortProtocol :: PortProtocol -> Doc ann
prettyPortProtocol PortProtocol {port, protocol} =
  listItem $
    vsep
      [ keyValue "port" (dquotes $ pretty port)
      , keyValue "protocol" (pretty protocol)
      ]

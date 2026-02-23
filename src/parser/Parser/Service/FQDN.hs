module Parser.Service.FQDN where

import KDL
import KDL.Decoder.Internal.Decoder

import Core.Model.FQDN

fqdnDecoder :: DecodeArrow NodeList () FQDN
fqdnDecoder = KDL.nodeWith "fqdn" $ do
  fqdn <- KDL.argWith KDL.text
  props <- KDL.remainingProps
  pure FQDN {fqdn, props}

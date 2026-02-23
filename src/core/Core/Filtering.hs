module Core.Filtering where

import Optics.Core

import Core.Filtering.Types
import Core.Model.FQDN
import Core.Model.Service

filterServiceOnAttributes
  :: FilterAction
  -> Service
  -> Service
filterServiceOnAttributes filterAction service =
  service
    & (#serviceInfo % #serviceFqdns %~ filter (\fqdn -> interpretFilter filterAction fqdn.props))

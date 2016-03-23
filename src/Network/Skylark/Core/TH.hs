-- |
-- Module:      Network.Skylark.API.Data.TH
-- Copyright:   (c) 2015 Mark Fine
--              (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License:     BSD3, MPLv2
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Template Haskell utilities module for Skylark API service.
--
-- Based partially on the text Network.PagerDuty.Internal.Options
-- module at https://github.com/brendanhay/pagerduty/.

module Network.Skylark.Core.TH
  ( deriveJSON
  , deriveJSONWith
  , deriveNullary
  ) where

import qualified Data.Aeson.TH                  as A
import           Data.Aeson.Types
import           Language.Haskell.TH
import           Network.Skylark.Core.Data.JSON

deriveJSONWith :: Options -> Name -> Q [Dec]
deriveJSONWith = A.deriveJSON

deriveJSON :: Name -> Q [Dec]
deriveJSON = deriveJSONWith underscored

deriveNullary :: Name -> Q [Dec]
deriveNullary = A.deriveJSON A.defaultOptions

-- |
-- Module:      Network.Skylark.Core.Data.JSON
-- Copyright:   (c) 2015 Mark Fine
--              (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License:     BSD3, MPLv2
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- JSON utilities module
--
-- Based partially on the text Network.PagerDuty.Internal.Options
-- module at https://github.com/brendanhay/pagerduty/.

module Network.Skylark.Core.Data.JSON
  ( underscored
  , allcaps
  ) where

import Data.Aeson.Types
import Network.Skylark.Core.Data.Text
import Network.Skylark.Core.Prelude

underscored :: Options
underscored = defaultOptions
  { fieldLabelModifier     = unsuffix . underscore . unprefix
  , constructorTagModifier = underscore
  , omitNothingFields      = True
  , allNullaryToStringTag  = True
  }

allcaps :: Options
allcaps = defaultOptions
  { fieldLabelModifier     = upper
  , constructorTagModifier = id
  , omitNothingFields      = True
  , allNullaryToStringTag  = True
  }

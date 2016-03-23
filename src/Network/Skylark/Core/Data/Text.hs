-- |
-- Module:      Network.Skylark.Core.Text
-- Copyright:   (c) 2015 Mark Fine
--              (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License:     BSD3, MPLv2
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Text utilities module.
--
-- Based partially on the text Network.PagerDuty.Internal.Options
-- module at https://github.com/brendanhay/pagerduty/.

module Network.Skylark.Core.Data.Text
  ( underscore
  , lower
  , upper
  , capitalize
  , unsuffix
  , unprefix
  ) where

import Data.Char
import Data.List
import Network.Skylark.Core.Prelude hiding (intercalate, map)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p = groupBy (const (not . p))

underscore :: String -> String
underscore = intercalate "_" . map lower . splitBy isUpper

lower :: String -> String
lower = map toLower

upper :: String -> String
upper = map toUpper

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

unsuffix :: String -> String
unsuffix = takeWhile (/= '\'')

unprefix :: String -> String
unprefix = dropWhile (not . isUpper)

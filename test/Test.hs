-- |
-- Module:      Test
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Test module for Skylark Core.

import           BasicPrelude
import qualified Test.Network.Skylark.Core.Conf     as Conf
import qualified Test.Network.Skylark.Core.DynamoDB as DynamoDB
import qualified Test.Network.Skylark.Core.Metrics  as Metrics
import qualified Test.Network.Skylark.Core.Retries  as Retries
import qualified Test.Network.Skylark.Core.Setup    as Setup
import qualified Test.Network.Skylark.Core.TChans   as TChans
import qualified Test.Network.Skylark.Core.Time     as Time
import qualified Test.Network.Skylark.Core.Types    as Types
import           Test.Tasty

tests :: TestTree
tests = testGroup "Tests"
  [ Conf.tests
  , DynamoDB.tests
  , Retries.tests
  , Metrics.tests
  , Setup.tests
  , TChans.tests
  , Types.tests
  , Time.tests
  ]

main :: IO ()
main = defaultMain tests

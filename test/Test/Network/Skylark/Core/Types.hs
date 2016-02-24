{-# OPTIONS  -fno-warn-orphans          #-}
-- |
-- Module:      Test.Network.Skylark.Core.TChans
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Test send and receive TChans module for Skylark Core.

module Test.Network.Skylark.Core.Types
  ( tests
  ) where

import Data.Time
import Network.Skylark.Core.Prelude
import Network.Skylark.Core.Types
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary UTCTime where
    arbitrary = do
      randomDay   <- choose (1, 29) :: Gen Int
      randomMonth <- choose (1, 12) :: Gen Int
      randomYear  <- choose (1980, 2015) :: Gen Integer
      randomTime  <- choose (0, 86401) :: Gen Int
      return $ UTCTime (fromGregorian randomYear randomMonth randomDay) (fromIntegral randomTime)

testUntxt :: TestTree
testUntxt =
  testGroup "Untxt tests"
    [ testProperty "UTCTime" $
        \t -> monadicIO $ do
          t' <- run $ untxt (txt t)
          assert $ (t :: UTCTime) == t'
    ]

tests :: TestTree
tests =
  testGroup "Untxt tests"
    [ testUntxt
    ]

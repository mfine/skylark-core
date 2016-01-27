{-# OPTIONS -fno-warn-orphans #-}

-- |
-- Module:      Test.Network.Skylark.Core.Retries
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Test retries module for Skylark Core.

module Test.Network.Skylark.Core.Retries
  ( tests
  ) where

import Control.Lens                 hiding (pre)
import Control.Monad.Catch
import Data.IORef
import Network.Skylark.Core.Prelude
import Network.Skylark.Core.Retries
import Network.Skylark.Core.Types
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary RetryState where
  arbitrary = RetryState <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

limits :: Ord a => Maybe a -> a -> a -> Bool
limits delay lo hi =
  maybe' delay False $ \delay' ->
    delay' >= lo && delay' <= hi

testPolicies :: TestTree
testPolicies =
  testGroup "Policy tests"
    [ testProperty "Constant" $
        \delay state -> monadicIO $ do
          delay' <- run $ runRetryPolicy constant $ state & rsDelay .~ delay
          assert $ delay' == delay
    , testProperty "Fixed delay" $
        \delay state -> monadicIO $ do
          delay' <- run $ runRetryPolicy (fixedDelay delay) state
          assert $ delay' == Just delay
    , testProperty "Exponential backoff" $
        \base count state -> monadicIO $ do
          delay <- run $ runRetryPolicy (exponentialBackoff base) $ state & rsCount .~ count
          let temp = base * 2 ^ count
          assert $ maybe' delay False (== temp)
    , testProperty "Full jitter" $
        \base count state -> monadicIO $ do
          delay <- run $ runRetryPolicy (fullJitter base) $ state & rsCount .~ count
          let temp = base * 2 ^ count
          assert $ limits delay 0 temp
    , testProperty "Equal jitter" $
        \base count state -> monadicIO $ do
          delay <- run $ runRetryPolicy (equalJitter base) $ state & rsCount .~ count
          let temp = base * 2 ^ count
          assert $ limits delay (div temp 2) temp
    , testProperty "Decorrelated jitter" $
        \base delay state -> monadicIO $ do
          delay' <- run $ runRetryPolicy (decorrelatedJitter base) $ state & rsDelay .~ Just delay
          let temp = max base delay * 3
          assert $ limits delay' base temp
    , testProperty "Decorrelated jitter (no delay)" $
        \base state -> monadicIO $ do
          delay' <- run $ runRetryPolicy (decorrelatedJitter base) $ state & rsDelay .~ Nothing
          let temp = base * 3
          assert $ limits delay' base temp
    ]

testPolicyModifiers :: TestTree
testPolicyModifiers =
  testGroup "Policy modifier tests"
    [ testProperty "Jitter" $
        \delay state -> monadicIO $ do
          delay' <- run $ runRetryPolicy (jitter constant) $ state & rsDelay .~ Just delay
          let temp = div delay 10
          assert $ limits delay' (delay - temp) (delay + temp)
    , testProperty "Jitter (no delay)" $
        \state -> monadicIO $ do
          delay <- run $ runRetryPolicy (jitter constant) $ state & rsDelay .~ Nothing
          assert $ isNothing delay
    , testProperty "Max count" $
        \delay count limit state -> monadicIO $ do
          delay' <- run $ runRetryPolicy (maxCount limit constant) $ state &
            rsCount .~ count &
            rsDelay .~ delay
          assert $ if count + 1 >= limit then isNothing delay' else delay' == delay
   , testProperty "Max delay" $
       \delay limit state -> monadicIO $ do
         delay' <- run $ runRetryPolicy (maxDelay limit constant) $ state & rsDelay .~ Just delay
         assert $ maybe' delay' False $ \delay'' -> delay'' == delay || delay'' == limit
   , testProperty "Max delay (no delay)" $
       \limit state -> monadicIO $ do
         delay <- run $ runRetryPolicy (maxDelay limit constant) $ state & rsDelay .~ Nothing
         assert $ isNothing delay
   , testProperty "Max total" $
       \delay total limit state -> monadicIO $ do
         delay' <- run $ runRetryPolicy (maxTotal limit constant) $ state &
           rsDelay .~ Just delay &
           rsTotal .~ total
         assert $ if total + delay >= limit then isNothing delay' else delay' == Just delay
   , testProperty "Max total (no delay)" $
       \total limit state -> monadicIO $ do
         delay <- run $ runRetryPolicy (maxTotal limit constant) $ state &
           rsDelay .~ Nothing &
           rsTotal .~ total
         assert $ isNothing delay
    ]

countAction :: (IORef (Small Word) -> IO ()) -> IO (Small Word)
countAction action = do
  counter <- newIORef 0
  action counter
  readIORef counter

countRetry :: Bool -> Small Word -> PropertyM IO (Small Word)
countRetry again count = do
  pre $ count > 0
  run $ countAction $ \counter ->
    retry (maxCount (getSmall count) $ fixedDelay 0)
      (const $ return again) $
      modifyIORef' counter (+ 1)

testRetry :: TestTree
testRetry =
  testGroup "Retry tests"
    [ testProperty "Again" $
        \count -> monadicIO $ do
          counter <- countRetry True count
          assert $ counter == count
    , testProperty "Not again" $
        \count -> monadicIO $ do
          counter <- countRetry False count
          assert $ counter == 1
    ]

recoverAllAction :: Bool -> Small Word -> IO () -> IORef (Small Word) -> IO ()
recoverAllAction again count action counter =
  recoverAll (maxCount (getSmall count) $ fixedDelay 0)
    (const $ return again) $ do
      modifyIORef' counter (+ 1)
      action

countRecoverAll :: Bool -> Small Word -> IO () -> PropertyM IO (Small Word)
countRecoverAll again count action = do
  pre $ count > 0
  run $ countAction $
    recoverAllAction again count action

countRecoverAll' :: Bool -> Small Word -> IO () -> PropertyM IO (Small Word)
countRecoverAll' again count action = do
  pre $ count > 0
  run $ countAction $
    handleAll (const $ return ()) . recoverAllAction again count action

testRecoverAll :: TestTree
testRecoverAll =
  testGroup "Recover All tests"
    [ testProperty "No exception" $
        \count -> monadicIO $ do
          counter <- countRecoverAll True count $ return ()
          assert $ counter == 1
    , testProperty "Exception again" $
        \count -> monadicIO $ do
          counter <- countRecoverAll' True count $ throwM $ userError "omg"
          assert $ counter == count
    , testProperty "Exception not again" $
        \count -> monadicIO $ do
          counter <- countRecoverAll' False count $ throwM $ userError "omg"
          assert $ counter == 1
    ]

tests :: TestTree
tests =
  testGroup "Retries tests"
    [ testPolicies
    , testPolicyModifiers
    , testRetry
    , testRecoverAll
    ]

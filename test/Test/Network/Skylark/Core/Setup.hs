-- |
-- Module:      Test.Network.Skylark.Core.Test
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Setup module for Skylark Core.

module Test.Network.Skylark.Core.Setup where

import BasicPrelude
import Control.Lens               hiding ((.=))
import Data.Aeson
import Data.Default
import Network.Skylark.Core.Conf
import Network.Skylark.Core.Setup
import Network.Skylark.Core.Types
import Paths_skylark_core
import Test.Tasty
import Test.Tasty.HUnit

testCtxInit :: TestTree
testCtxInit =
  testGroup "New application context"
    [ testCase "Empty configuration" $
        void (newCtx mempty "") `catch` \(MandatoryConfException _) -> return ()
    , testCase "Default configuration" $ do
        i <- getDataFileName "conf/info.yaml" >>= getDataFile
        let d = def & confAppName  .~ Just "Testing"
        t <- newCtx d (i ^. ifTag)
        t ^. ctxPrefix @?= object [("name", String "Testing"), ("tag", String "sample")]
    ]

tests :: TestTree
tests =
  testGroup "Setup tests"
    [ testCtxInit
    ]

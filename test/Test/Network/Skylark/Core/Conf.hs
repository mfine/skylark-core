{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS  -fno-warn-orphans          #-}

-- |
-- Module:      Test.Network.Skylark.Core.Conf
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Test configuration Options module for Skylark Core.

module Test.Network.Skylark.Core.Conf
  ( tests
  ) where

import BasicPrelude
import Control.Applicative
import Control.Lens                   hiding (set, (.=))
import Control.Monad.Logger
import Data.Default
import Network.Skylark.Core.Conf
import Network.Skylark.Core.Types
import Options.Applicative
import Paths_skylark_core
import System.Environment
import System.Envy
import Test.Network.Skylark.Core.Test
import Test.QuickCheck
import Test.QuickCheck.Instances      ()
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck


{-# ANN module ("HLint: ignore Reduce duplication"::String) #-}

parse :: [String] -> Maybe Conf
parse = getParseResult . execParserPure (prefs idm) (parser parseConf)

testGeneral :: TestTree
testGeneral =
  testGroup "General arguments"
    [ testCase "None" $
        parse [] @?= Just confTest
    , testCase "Junk" $
        parse ["junk"] @?= Nothing
    ]

testConfFile :: TestTree
testConfFile =
  testGroup "Conf file argument"
    [ testCase "Short" $
        parse ["-c", "conf/prod.yaml"] @?= Just testOptions
    , testCase "Long" $
        parse ["--conf-file", "conf/prod.yaml"] @?= Just testOptions
    ] where
      testOptions = confTest & confFile .~ Just "conf/prod.yaml"

testPort :: TestTree
testPort =
  testGroup "Port argument"
    [ testCase "Short" $
        parse ["-p", "3000"] @?= Just testOptions
    , testCase "Long" $
        parse ["--port", "3000"] @?= Just testOptions
    , testCase "Bad integer" $
        parse ["-p", "junk"] @?= Nothing
    ] where
      testOptions = confTest & confPort .~ Just 3000

testTimeout :: TestTree
testTimeout =
  testGroup "Timeout argument"
    [ testCase "Short" $
        parse ["-t", "1"] @?= Just testOptions
    , testCase "Long" $
        parse ["--timeout", "1"] @?= Just testOptions
    , testCase "Bad integer" $
        parse ["-t", "junk"] @?= Nothing
    ] where
      testOptions = confTest & confTimeout .~ Just 1

testLogLevel :: TestTree
testLogLevel =
  testGroup "Log level argument"
    [ testCase "Debug" $
        parse ["--log-level", "debug"] @?= Just (testOptions LevelDebug)
    , testCase "Info" $
        parse ["--log-level", "info"] @?= Just (testOptions LevelInfo)
    , testCase "Warn" $
        parse ["--log-level", "warn"] @?= Just (testOptions LevelWarn)
    , testCase "Error" $
        parse ["--log-level", "error"] @?= Just (testOptions LevelError)
    , testCase "Other" $
        parse ["--log-level", "wut"] @?= Just (testOptions (LevelOther "wut"))
    ] where
      testOptions level = confTest & confLogLevel .~ Just level

testMetrics :: TestTree
testMetrics =
  testGroup "Metrics flag"
    [ testCase "Long" $
        parse ["--metrics"] @?= Just testOptions
    ] where
      testOptions = confTest & confMetrics .~ Just True

--------------------------------------------------------------------------------
-- Environmental parsing stuff

testEnv :: TestTree
testEnv =
  testGroup "Environmental configuration unit test"
    [ testCase "Empty configuration" $ do
        unsetEnv "SKYLARK_CONF_FILE"
        unsetEnv "SKYLARK_PORT"
        unsetEnv "SKYLARK_TIMEOUT"
        unsetEnv "SKYLARK_LOG_LEVEL"
        c <- decode
        c @?= Just Conf
          { _confFile     = Nothing
          , _confPort     = Nothing
          , _confTimeout  = Nothing
          , _confLogLevel = Nothing
          , _confAppName  = Nothing
          , _confMetrics  = Nothing
          , _confDdbPort  = Nothing
          }
    , testCase "Port and Timeout" $ do
        unsetEnv "SKYLARK_CONF_FILE"
        setEnv "SKYLARK_PORT" "1"
        setEnv "SKYLARK_TIMEOUT" "1"
        unsetEnv "SKYLARK_LOG_LEVEL"
        c <- decode
        c @?= Just Conf
          { _confFile     = Nothing
          , _confPort     = Just 1
          , _confTimeout  = Just 1
          , _confLogLevel = Nothing
          , _confAppName  = Nothing
          , _confMetrics  = Nothing
          , _confDdbPort  = Nothing
          }
     , testCase "String value" $ do
        setEnv "SKYLARK_CONF_FILE" "l"
        unsetEnv "SKYLARK_PORT"
        setEnv "SKYLARK_TIMEOUT" "1"
        unsetEnv "SKYLARK_LOG_LEVEL"
        c <- decode
        c @?= Just Conf
          { _confFile     = Just "l"
          , _confPort     = Nothing
          , _confTimeout  = Just 1
          , _confLogLevel = Nothing
          , _confAppName  = Nothing
          , _confMetrics  = Nothing
          , _confDdbPort  = Nothing
          }
     , testCase "LevelInfo" $ do
        unsetEnv "SKYLARK_CONF_FILE"
        unsetEnv "SKYLARK_PORT"
        unsetEnv "SKYLARK_TIMEOUT"
        setEnv "SKYLARK_LOG_LEVEL" "info"
        c <- decode
        c @?= Just Conf
          { _confFile     = Nothing
          , _confPort     = Nothing
          , _confTimeout  = Nothing
          , _confLogLevel = Just LevelInfo
          , _confAppName  = Nothing
          , _confMetrics  = Nothing
          , _confDdbPort  = Nothing
          }
     , testCase "LevelOther" $ do
        unsetEnv "SKYLARK_CONF_FILE"
        unsetEnv "SKYLARK_PORT"
        unsetEnv "SKYLARK_TIMEOUT"
        setEnv "SKYLARK_LOG_LEVEL" "other"
        c <- decode
        c @?= Just Conf
          { _confFile     = Nothing
          , _confPort     = Nothing
          , _confTimeout  = Nothing
          , _confLogLevel = Just (LevelOther "other")
          , _confAppName  = Nothing
          , _confMetrics  = Nothing
          , _confDdbPort  = Nothing
          }
    ]

testDataFileFetch :: TestTree
testDataFileFetch =
  testGroup "Testing reading of datafile"
    [ testCase "Existing test data file" $ do
        c <- getDataFile "conf/testing.yaml"
        c @?= Conf
          { _confFile     = Nothing
          , _confPort     = Just 3031
          , _confTimeout  = Just 121
          , _confLogLevel = Just LevelDebug
          , _confAppName  = Nothing
          , _confMetrics  = Nothing
          , _confDdbPort  = Nothing
          }
    , testCase "Existing data file" $ do
        c <- getDataFile "conf/dev.yaml"
        c @?= Conf
          { _confFile     = Nothing
          , _confPort     = Just 3030
          , _confTimeout  = Just 120
          , _confLogLevel = Just LevelInfo
          , _confAppName  = Nothing
          , _confMetrics  = Nothing
          , _confDdbPort  = Nothing
          }
    ]

testConfMonoid :: TestTree
testConfMonoid =
  testGroup "Testing monoid merging of Conf's Maybe fields"
    [ testCase "Both a and b are Nothings" $ do
        let a = confTest
            b = a
        a <> b @?= a
    , testCase "Both a and b have one Just" $ do
        let a = confTest & confPort .~ Just 1
            b = a
        a <> b @?= a
    , testCase "b has a Just" $ do
        let a = confTest
            b = a & confPort .~ Just 1
        a <> b @?= b
    , testCase "a has a Just" $ do
        let a = confTest & confPort .~ Just 1
            b = a & confPort .~ Nothing
        a <> b @?= a
    , testCase "Two separate Just fields are in the merged" $ do
        let a = confTest & confPort .~ Just 1
            b = a & confTimeout .~ Just 120
        a <> b @?= b
    , testCase "Empty conf is ignored" $ do
        let a = confTest & confPort .~ Just 1
            b = mempty
        a <> b @?= a
        b <> a @?= a
    ]

testGetConf :: TestTree
testGetConf =
  testGroup "Test parsing of a complete configuration"
    [ testCase "Sanity test on parsing of configuration with defaults" $ do
        unsetEnv "SKYLARK_CONF_FILE"
        unsetEnv "SKYLARK_LOG_LEVEL"
        unsetEnv "SKYLARK_PORT"
        c <- getConf parseConf getDataFileName
        c @?= (def & confPort .~ Just 3030)
    , testCase "Sanity test on parsing of configuration with a non-default" $ do
        unsetEnv "SKYLARK_CONF_FILE"
        unsetEnv "SKYLARK_LOG_LEVEL"
        setEnv "SKYLARK_PORT" "2222"
        c <- getConf parseConf getDataFileName
        c @?= (def & confPort .~ Just 2222)
    ]

instance Arbitrary LogLevel where
  arbitrary = do
    n <- choose (0, 4) :: Gen Int
    return $ case n of
      0 -> LevelDebug
      1 -> LevelInfo
      2 -> LevelWarn
      3 -> LevelError
      4 -> LevelOther "other"
      _ -> error "Impossible match!"

maybeGen :: Gen a -> Gen (Maybe a)
maybeGen x = oneof [ pure Nothing , Just <$> x ]

nonEmptyString :: Gen String
nonEmptyString = listOf1 arbitrary `suchThat` (not . ('\NUL' `elem`))

instance Arbitrary Conf where
  arbitrary = Conf                      <$>
    maybeGen nonEmptyString             <*>
    maybeGen arbitrary                  <*>
    maybeGen arbitrary                  <*>
    maybeGen arbitrary                  <*>
    maybeGen (liftA txt nonEmptyString) <*>
    maybeGen arbitrary                  <*>
    maybeGen arbitrary

testEnvProperties :: TestTree
testEnvProperties = testGroup "(checked by QuickCheck)"
  [ testProperty "Log Level environmental configuration" $
      \(x :: LogLevel) -> Just x == fromVar (toVar x)
  , testProperty "Configuration environmental configuration" $
      \(c :: Conf) -> monadicIO $ do
        res <- run $ do
                  let Conf{..} = c
                      set k = maybe (unsetEnv k) (setEnv k . toVar)
                  set "SKYLARK_CONF_FILE" _confFile
                  set "SKYLARK_PORT"      _confPort
                  set "SKYLARK_TIMEOUT"   _confTimeout
                  set "SKYLARK_LOG_LEVEL" _confLogLevel
                  set "SKYLARK_APP_NAME"  _confAppName
                  set "SKYLARK_METRICS"   _confMetrics
                  set "SKYLARK_DDB_PORT"  _confDdbPort
                  decodeEnv
        Test.QuickCheck.Monadic.assert $ res == Right c
  ]

testProperties :: TestTree
testProperties = testGroup "Properties" [ testEnvProperties ]

tests :: TestTree
tests =
  testGroup "Options tests"
    [ testGeneral
    , testConfFile
    , testPort
    , testTimeout
    , testLogLevel
    , testMetrics
    , testEnv
    , testDataFileFetch
    , testConfMonoid
    , testGetConf
    , testProperties
    ]

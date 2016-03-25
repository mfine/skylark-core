-- |
-- Module:      Network.Skylark.Core.Setup
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Shared Setup module: this contains things like utilities for
-- setting up application context.

module Network.Skylark.Core.Setup
  ( checkHealth
  , newCtx
  , newSettings
  , withHealthCheck
  ) where

import Control.Concurrent.Async
import Control.Lens
import Control.Monad.Trans.AWS                 hiding (timeout)
import Data.Text
import Data.Time
import Network.AWS.DynamoDB
import Network.Skylark.Core
import Network.Skylark.Core.Async
import Network.Skylark.Core.Conf
import Network.Skylark.Core.Constants
import Network.Skylark.Core.Prelude
import Network.Skylark.Core.Providers.StatGrab
import Network.Skylark.Core.Trace
import Network.Wai.Handler.Warp

--------------------------------------------------------------------------------
-- Setup for application context

-- | Setup WAI settings
--
newSettings :: Int -> Int -> Settings
newSettings port timeout =
  setPort port       $
  setTimeout timeout
  defaultSettings

-- | Initialize application context from configuration.
--
newCtx :: Conf         -- ^ Service configuration
       -> Text         -- ^ Git tag
       -> IO Ctx
newCtx c tag = do
  env     <- newEnv Oregon $ FromEnv awsAccessKey awsSecretKey Nothing
  name    <- mandatory "app-name" $ c ^. confAppName
  port    <- mandatory "port"     $ c ^. confPort
  timeout <- mandatory "timeout"  $ c ^. confTimeout
  let _ctxConf     = c
      _ctxPreamble = preamble name
      _ctxSettings = newSettings port timeout
      _ctxClock    = getCurrentTime
      _ctxEnv      = if not (fromMaybe False (_confLocalDdb c)) then env else
                       env & configure (setEndpoint False "localhost" 8000 dynamoDB)
  logLevel  <- mandatory "log-level" $ c ^. confLogLevel
  _ctxLog   <- newStderrTrace logLevel
  _ctxStart <- _ctxClock
  return Ctx {..} where
    preamble name =
      sformat ("n=" % stext % " t=" % stext) name tag

-- | Core context with some periodic health checking.
--
withHealthCheck :: HasCtx e => e -> IO b -> IO b
withHealthCheck c action =
  withAsync action $ \a ->
    withPeriodic healthCheckInterval (checkHealth c) $ \b ->
      wait b >> wait a

--------------------------------------------------------------------------------
-- Setup for health monitoring

-- Emit a health check metrics
--
checkHealth :: HasCtx e => e -> IO ()
checkHealth c =
  runCoreIO c $ do
    gr <- getEventGroup
    s  <- runStats sampleStats
    mapM_ traceMetric $ measure gr s

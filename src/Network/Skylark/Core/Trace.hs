{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module:      Network.Skylark.Core.Trace
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Trace module for Skylark Core.

module Network.Skylark.Core.Trace
  ( newStderrTrace
  , newStdoutTrace
  , traceNull
  , metricDebug
  , metricInfo
  , metricWarn
  , metricError
  , emitDebug
  , emitInfo
  , emitWarn
  , emitError
  ) where

import Control.Lens                 hiding ((.=))
import Control.Monad.Logger
import Data.Aeson
import Data.Aeson.Encode
import Data.ByteString.Builder
import Formatting
import Network.Skylark.Core.Prelude
import Network.Skylark.Core.Types
import System.Log.FastLogger

--------------------------------------------------------------------------------
-- Text logging

traceLevel :: LogLevel -> LoggerSet -> Log
traceLevel level ls _loc _source level' s =
  unless (level' < level) $ do
    pushLogStr ls s
    flushLogStr ls

newStderrTrace :: LogLevel -> IO Log
newStderrTrace level = do
  ls <- newStderrLoggerSet defaultBufSize
  return $ traceLevel level ls

newStdoutTrace :: LogLevel -> IO Log
newStdoutTrace level = do
  ls <- newStdoutLoggerSet defaultBufSize
  return $ traceLevel level ls

traceNull :: Log
traceNull _loc _source _level _s =
  return ()

--------------------------------------------------------------------------------
-- Event logging: emit JSON and metrics to the application log.

metric :: MonadCore e m => (Text -> m ()) -> Metric -> m ()
metric logN m = do
  metrics <- view confMetrics
  when (fromMaybe False metrics) $ do
    g <- view ctxGroup
    logN $ sformat (":" % stext % "." % stext) g (txt m)

metricDebug :: MonadCore e m => Metric -> m ()
metricDebug = metric logDebugN

metricInfo :: MonadCore e m => Metric -> m ()
metricInfo = metric logInfoN

metricWarn :: MonadCore e m => Metric -> m ()
metricWarn = metric logWarnN

metricError :: MonadCore e m => Metric -> m ()
metricError = metric logErrorN

newPrefix :: MonadCore e m => m Value
newPrefix = do
  clock  <- view ctxClock
  time   <- liftIO clock
  prefix <- view ctxPrefix
  return $ mergeObjects prefix $ object [ "time" .= time ]

emit :: (MonadCore e m, ToMetric a) => (Text -> m ()) -> a -> m ()
emit logN e = do
  prefix <- newPrefix
  logN $ txt $ toLazyByteString $ encodeToBuilder $
    mergeObjects prefix $ toJSON e
  maybe (return ()) (metric logN) (toMetric e)

emitDebug :: (MonadCore e m, ToMetric a) => a -> m ()
emitDebug = emit logDebugN

emitInfo :: (MonadCore e m, ToMetric a) => a -> m ()
emitInfo = emit logInfoN

emitWarn :: (MonadCore e m, ToMetric a) => a -> m ()
emitWarn = emit logWarnN

emitError :: (MonadCore e m, ToMetric a) => a -> m ()
emitError = emit logErrorN


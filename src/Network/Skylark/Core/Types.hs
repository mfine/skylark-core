{-# OPTIONS  -fno-warn-orphans          #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
-- Module:      Network.Skylark.Core.Types
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Types module for Skylark Core.

module Network.Skylark.Core.Types where

import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Lens                 hiding ((.=))
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Error.Class
import           Control.Monad.Logger
import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS      hiding (LogLevel, Request)
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Resource
import           Data.Aeson                   hiding ((.!=), (.=))
import qualified Data.ByteString.Lazy         as LBS
import           Data.CaseInsensitive
import           Data.Default
import           Data.Monoid
import           Data.Text                    (pack, unpack)
import           Data.Text.Lazy               (toStrict)
import           Data.Text.Lazy.Builder       hiding (fromText)
import           Data.Time
import           Data.UUID
import qualified Data.UUID                    as UUID
import           Network.AWS.DynamoDB
import           Network.HTTP.Types
import           Network.Skylark.Core.Lens.TH
import           Network.Skylark.Core.Prelude hiding (mask, uninterruptibleMask)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           System.Envy
import           System.Statgrab

--------------------------------------------------------------------------------
-- Service configuration

instance Var LogLevel where
  toVar LevelDebug     = "debug"
  toVar LevelInfo      = "info"
  toVar LevelWarn      = "warn"
  toVar LevelError     = "error"
  toVar (LevelOther s) = unpack s

  fromVar "debug" = return LevelDebug
  fromVar "info"  = return LevelInfo
  fromVar "warn"  = return LevelWarn
  fromVar "error" = return LevelError
  fromVar s       = return $ LevelOther (pack s)

-- | A record type representing full or partial configuration of an
-- HTTP service. Remaining unspecified fields are filled in with the
-- default values from Default.
--
data Conf = Conf
  { _confFile         :: Maybe String   -- ^ Service configuration file location
  , _confPort         :: Maybe Int      -- ^ Port to listen on
  , _confTimeout      :: Maybe Int      -- ^ Connection timeout (sec)
  , _confLogLevel     :: Maybe LogLevel -- ^ Logging level
  , _confAppName      :: Maybe Text     -- ^ Name of the application
  , _confMetrics      :: Maybe Bool     -- ^ Enable metrics collection
  , _confDdbLocalPort :: Maybe Int      -- ^ Port of local DDB instance (if testing)
                                        -- If unset, default to prod configuration.
  } deriving ( Eq, Show )

$(makeClassy ''Conf)

type MonadConf a =
  ( HasConf  a
  , Default  a
  , FromJSON a
  , FromEnv  a
  , Monoid   a
  )

data ConfException = MandatoryConfException String
  deriving ( Show, Eq )

instance Exception ConfException

instance Default Conf where
  def = Conf
    { _confFile          = Just "conf/dev.yaml"
    , _confPort          = Just 5000
    , _confTimeout       = Just 120
    , _confLogLevel      = Just LevelInfo
    , _confAppName       = Nothing
    , _confMetrics       = Nothing
    , _confDdbLocalPort  = Nothing
    }

instance FromJSON Conf where
  parseJSON (Object v) =
    Conf                     <$>
      v .:? "conf-file"      <*>
      v .:? "port"           <*>
      v .:? "timeout"        <*>
      v .:? "log-level"      <*>
      v .:? "app-name"       <*>
      v .:? "metrics"        <*>
      v .:? "ddb-local-port"
  parseJSON _ = mzero

instance FromEnv Conf where
  fromEnv =
    Conf                                <$>
      envMaybe "SKYLARK_CONF_FILE"      <*>
      envMaybe "SKYLARK_PORT"           <*>
      envMaybe "SKYLARK_TIMEOUT"        <*>
      envMaybe "SKYLARK_LOG_LEVEL"      <*>
      envMaybe "SKYLARK_APP_NAME"       <*>
      envMaybe "SKYLARK_METRICS"        <*>
      envMaybe "SKYLARK_DDB_LOCAL_PORT"

instance Monoid Conf where
  mempty = Conf
    { _confFile          = Nothing
    , _confPort          = Nothing
    , _confTimeout       = Nothing
    , _confLogLevel      = Nothing
    , _confAppName       = Nothing
    , _confMetrics       = Nothing
    , _confDdbLocalPort  = Nothing
    }

  mappend a b = Conf
    { _confFile          = merge _confFile a b
    , _confPort          = merge _confPort a b
    , _confTimeout       = merge _confTimeout a b
    , _confLogLevel      = merge _confLogLevel a b
    , _confAppName       = merge _confAppName a b
    , _confMetrics       = merge _confMetrics a b
    , _confDdbLocalPort  = merge _confDdbLocalPort a b
    }

-- | Given a record field accessor. return the second non-Nothing
-- Value for a record field.
--
merge :: (a -> Maybe b) -> a -> a -> Maybe b
merge f a b = getLast $! (mappend `on` (Last . f)) a b

--------------------------------------------------------------------------------
-- Application context

type Log = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

newtype CoreT e m a = CoreT
  { unCoreT :: LoggingT (AWST' e m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadThrow
             , MonadCatch
             , MonadMask
             , MonadLogger
             )

data Ctx = Ctx
  { _ctxConf     :: Conf
  , _ctxEnv      :: Env
  , _ctxLog      :: Log
  , _ctxGroup    :: Text
  , _ctxPrefix   :: Value
  , _ctxSettings :: Settings
  , _ctxClock    :: IO UTCTime
  , _ctxStart    :: UTCTime
  }

$(makeClassyConstraints ''Ctx [''HasConf, ''HasEnv])

instance HasEnv Ctx where
  environment = ctxEnv

instance HasConf Ctx where
  conf = ctxConf

instance MonadBase b m => MonadBase b (CoreT r m) where
  liftBase = liftBaseDefault

instance MonadTrans (CoreT r) where
  lift = CoreT . lift . lift

instance MonadResource m => MonadResource (CoreT r m) where
  liftResourceT = lift . liftResourceT

instance Monad m => MonadReader r (CoreT r m) where
  ask     = CoreT ask
  local f = CoreT . local f . unCoreT
  reader  = CoreT . reader

instance MonadError e m => MonadError e (CoreT r m) where
  throwError     = lift . throwError
  catchError m f = CoreT (unCoreT m `catchError` (unCoreT . f))

instance MonadRandom m => MonadRandom (CoreT r m) where
  getRandom   = lift getRandom
  getRandomR  = lift . getRandomR
  getRandoms  = lift getRandoms
  getRandomRs = lift . getRandomRs

instance MonadRandom m => MonadRandom (ResourceT m) where
  getRandom   = lift getRandom
  getRandoms  = lift getRandoms
  getRandomR  = lift . getRandomR
  getRandomRs = lift . getRandomRs

type MonadCore e m =
  ( AWSConstraint e m
  , HasCtx e
  , MonadLogger m
  , MonadRandom m
  )

type MonadMap k m =
  ( Eq k
  , Hashable k
  , MonadIO m
  , MonadMask m
  )

type AttributeValueMap = HashMap Text AttributeValue

data Upsert = Upsert
  { _upsertTable :: Text
  , _upsertTime  :: UTCTime
  , _upsertKey   :: AttributeValueMap
  , _upsertExprs :: [Text]
  , _upsertVals  :: AttributeValueMap
  } deriving ( Eq, Show )

$(makeClassy ''Upsert)

--------------------------------------------------------------------------------
-- Common type classes

class Txt a where
  txt :: a -> Text

instance Txt String where
  txt = pack

instance Txt ByteString where
  txt = decodeUtf8

instance Txt LBS.ByteString where
  txt = decodeUtf8 . LBS.toStrict

instance Txt Builder where
  txt = toStrict . toLazyText

instance Txt Double where
  txt = show

instance Txt Integer where
  txt = show

instance Txt Word8 where
  txt = show

instance Txt Word32 where
  txt = show

instance Txt Int32 where
  txt = show

instance Txt UUID where
  txt = toText

instance Txt UTCTime where
  txt time =
    txt $ formatTime defaultTimeLocale "%FT%T%z" time

instance Txt HeaderName where
  txt = decodeUtf8 . original

instance Txt SomeException where
  txt e = "'" <> show e <> "'"

instance Txt Request where
  txt req =
    sformat ("method=" % stext % " path=" % stext)
      (txt $ requestMethod req) (txt $ rawPathInfo req)

instance Txt a => Txt (Maybe a) where
  txt (Just a) = txt a
  txt Nothing  = "null"

class UnTxt a where
  untxt :: (Monad m) => Text -> m a

instance UnTxt String where
  untxt = return . unpack

instance UnTxt UTCTime where
  untxt s =
    untxt s >>= parseTimeM False defaultTimeLocale "%FT%T%z"

instance UnTxt ByteString where
  untxt = return . encodeUtf8

instance UnTxt UUID where
  untxt = maybe (fail "could not parse UUID") return . UUID.fromText

instance ToJSON UUID where
  toJSON = toJSON . toText

instance FromJSON UUID where
  parseJSON (String s) = maybe mzero return (fromText s)
  parseJSON _ = mzero

instance FromJSON LogLevel where
  parseJSON (String "debug") = return LevelDebug
  parseJSON (String "info")  = return LevelInfo
  parseJSON (String "warn")  = return LevelWarn
  parseJSON (String "error") = return LevelError
  parseJSON (String s)       = return $ LevelOther s
  parseJSON _                = mzero

-- | Git tag version in information file.
--
newtype InfoFile = InfoFile
  { _ifTag :: Text
  }

$(makeLenses ''InfoFile)

instance FromJSON InfoFile where
  parseJSON (Object v) =
    InfoFile     <$>
      v .: "tag"
  parseJSON _ = mzero

-- Merge JSON objects into a single JSON object.
--
mergeObjects :: Value -> Value -> Value
mergeObjects (Object o1) (Object o2) = Object $ o1 <> o2
mergeObjects _           (Object o2) = Object o2
mergeObjects (Object o1)  _          = Object o1
mergeObjects _ v                     = v

--------------------------------------------------------------------------------
-- Retries

data RetryState = RetryState
  { _rsCount :: Word
  , _rsDelay :: Maybe Word
  , _rsTotal :: Word
  } deriving ( Eq, Show )

$(makeLenses ''RetryState)

instance Default RetryState where
  def = RetryState
    { _rsCount = 0
    , _rsDelay = Nothing
    , _rsTotal = 0
    }

newtype RetryPolicy = RetryPolicy
  { runRetryPolicy :: RetryState -> IO (Maybe Word)
  }

instance Monoid RetryPolicy where
  mempty = RetryPolicy $ \state ->
    return $ state ^. rsDelay

  mappend policyA policyB = RetryPolicy $ \state -> do
    delayA <- runRetryPolicy policyA state
    delayB <- runRetryPolicy policyB state
    return $ max <$> delayA <*> delayB

--------------------------------------------------------------------------------
-- TChans

type TVarList a = TVar (TList a)

data TList a = TNil | TNull | TCons a {-# UNPACK #-} !(TVarList a)

data TWChan a = TWChan {-# UNPACK #-} !(TVar Word)
                       {-# UNPACK #-} !(TVar (TVarList a))
                       {-# UNPACK #-} !(TVar (TVarList a))
  deriving (Eq, Typeable)

data TRChan a = TRChan {-# UNPACK #-} !(TVar (TVarList a))
                       {-# UNPACK #-} !(TVar (TVarList a))
  deriving (Eq, Typeable)

data TWCChan a = TWCChan {-# UNPACK #-} !(TVar Bool)
                         {-# UNPACK #-} !(TWChan a)
  deriving (Eq, Typeable)

data TRCChan a = TRCChan {-# UNPACK #-} !(TVar Bool)
                         {-# UNPACK #-} !(TRChan a)
  deriving (Eq, Typeable)

--------------------------------------------------------------------------------
-- MVars

type Barrier = MVar ()

--------------------------------------------------------------------------------
-- Events: logging and metrics monitoring
--
-- The following describes a notion of an event that can be serialized
-- out to a text log entry and text log entry corresponding to an
-- metric. The intention here is that a logged events are collected
-- with a log collector running on the host and forwarded to an ETL
-- (e.g., AWS Kinesis, if running on AWS), which then dispatches
-- logged events to another consumer (e.g., statsD for metrics logs.)
--
-- The model here is based on ghc-events and
-- https://github.com/brendanhay/network-metrics/.

-- | Metrics
--
-- These types are agnostic as to the metrics collector used, but are
-- largely inspired by the types provided by StatsD (see:
-- https://github.com/etsy/statsd/blob/master/docs/metric_types.md).
-- Each of these metrics can be sampled with some annotation.
--
data Metric =
  -- | A simple counter, set to 0 at each flush
    Counter ByteString Integer
  -- | A timing interval
  | Timer   ByteString Double
  -- | An arbitrary value that can be recorded
  | Gauge   ByteString Double
  -- | Counts unique occurrences of events between flushes.
  | Set     ByteString Double
    deriving (Show, Eq)

-- | Class of measurable instances.
--
class Measurable a where
  -- | Create a metric from some instance under some grouping
  -- A group typically uniquely identifies a host and an application.
  --
  measure :: a -> [Metric]

type SystemStat =
  ( Host
  , Memory
  , Load
  , [DiskIO]
  , [NetworkIO]
  , [FileSystem]
  )

instance Txt Metric where
  txt (Counter b v) = sformat (stext % ":" % stext % "|c") (txt b) (txt v)
  txt (Timer   b v) = sformat (stext % ":" % stext % "|t") (txt b) (txt v)
  txt (Gauge   b v) = sformat (stext % ":" % stext % "|g") (txt b) (txt v)
  txt (Set     b v) = sformat (stext % ":" % stext % "|s") (txt b) (txt v)

instance Measurable Host where
  measure Host{..} =
    [ Gauge "hostUptime" $ realToFrac hostUptime ]

instance Measurable Memory where
  measure Memory{..} =
    [ Gauge "memTotal"        $ fromIntegral memTotal
    , Gauge "memFreeFraction" $ fromIntegral memFree / fromIntegral memTotal
    , Gauge "memFree"         $ fromIntegral memFree
    , Gauge "memUsedFraction" $ fromIntegral memUsed / fromIntegral memTotal
    , Gauge "memUsed"         $ fromIntegral memUsed
    , Gauge "memCache"        $ fromIntegral memCache
    ]

instance Measurable Load where
  measure Load{..} =
    [ Gauge "load1"  load1
    , Gauge "load5"  load5
    , Gauge "load15" load15
    ]

instance Measurable DiskIO where
  measure DiskIO{..} =
    [ Gauge (diskName <> ".diskRead")  $ fromIntegral diskRead
    , Gauge (diskName <> ".diskWrite") $ fromIntegral diskWrite
    ]

instance Measurable a => Measurable [a] where
  measure = concatMap measure

instance Measurable NetworkIO where
  measure NetworkIO{..} =
    [ Gauge (ifaceIOName <> ".ifaceTX")         $ fromIntegral ifaceTX
    , Gauge (ifaceIOName <> ".ifaceRX")         $ fromIntegral ifaceRX
    , Gauge (ifaceIOName <> ".ifaceIPackets")   $ fromIntegral ifaceIPackets
    , Gauge (ifaceIOName <> ".ifaceOPackets")   $ fromIntegral ifaceOPackets
    , Gauge (ifaceIOName <> ".ifaceIErrors")    $ fromIntegral ifaceIErrors
    , Gauge (ifaceIOName <> ".ifaceCollisions") $ fromIntegral ifaceCollisions
    ]

instance Measurable FileSystem where
  measure FileSystem{..} =
    [ Gauge (fsDeviceName <> ".fsUsed")         $ fromIntegral fsUsed
    , Gauge (fsDeviceName <> ".fsFree")         $ fromIntegral fsFree
    , Gauge (fsDeviceName <> ".fsUsedFraction") $ fromIntegral fsUsed / fromIntegral fsSize
    , Gauge (fsDeviceName <> ".fsFreeFraction") $ fromIntegral fsFree / fromIntegral fsSize
    ]

instance Measurable SystemStat where
  measure (h, m, l, d, n, f) =
    measure h <>
    measure m <>
    measure l <>
    measure d <>
    measure n <>
    measure f

class ToJSON a => ToMetric a where
  toMetric :: a -> Maybe Metric

instance MonadMask m => MonadMask (EitherT err m) where
  mask act = EitherT $ mask $ \restore ->
    runEitherT $ act (EitherT . restore . runEitherT)

  uninterruptibleMask act = EitherT $ uninterruptibleMask $ \restore ->
    runEitherT $ act (EitherT . restore . runEitherT)

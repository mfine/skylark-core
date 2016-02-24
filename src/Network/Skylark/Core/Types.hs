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

import Control.Concurrent.STM
import Control.Lens                 hiding ((.=))
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.Logger
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Trans.AWS      hiding (LogLevel, Request)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Resource
import Data.Aeson                   hiding ((.!=), (.=))
import Data.Aeson.Types
import Data.CaseInsensitive
import Data.Default
import Data.Monoid
import Data.Text                    (pack, unpack)
import Data.Text.Lazy               (toStrict)
import Data.Text.Lazy.Builder       hiding (fromText)
import Data.Time
import Data.UUID
import Network.AWS.DynamoDB
import Network.HTTP.Types
import Network.Skylark.Core.Lens.TH
import Network.Skylark.Core.Prelude hiding (mask, uninterruptibleMask)
import Network.Wai
import Network.Wai.Handler.Warp
import System.Envy
import System.Statgrab

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
  { _confFile     :: Maybe String   -- ^ Service configuration file location
  , _confPort     :: Maybe Int      -- ^ Port to listen on
  , _confTimeout  :: Maybe Int      -- ^ Connection timeout (sec)
  , _confLogLevel :: Maybe LogLevel -- ^ Logging level
  , _confAppName  :: Maybe Text     -- ^ Name of the application
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
    { _confFile     = Just "conf/dev.yaml"
    , _confPort     = Just 5000
    , _confTimeout  = Just 120
    , _confLogLevel = Just LevelInfo
    , _confAppName  = Nothing
    }

instance FromJSON Conf where
  parseJSON (Object v) =
    Conf                <$>
      v .:? "conf-file" <*>
      v .:? "port"      <*>
      v .:? "timeout"   <*>
      v .:? "log-level" <*>
      v .:? "app-name"
  parseJSON _ = mzero

instance FromEnv Conf where
  fromEnv =
    Conf                           <$>
      envMaybe "SKYLARK_CONF_FILE" <*>
      envMaybe "SKYLARK_PORT"      <*>
      envMaybe "SKYLARK_TIMEOUT"   <*>
      envMaybe "SKYLARK_LOG_LEVEL" <*>
      envMaybe "SKYLARK_APP_NAME"

instance Monoid Conf where
  mempty = Conf
    { _confFile     = Nothing
    , _confPort     = Nothing
    , _confTimeout  = Nothing
    , _confLogLevel = Nothing
    , _confAppName  = Nothing
    }

  mappend a b = Conf
    { _confFile     = merge _confFile a b
    , _confPort     = merge _confPort a b
    , _confTimeout  = merge _confTimeout a b
    , _confLogLevel = merge _confLogLevel a b
    , _confAppName  = merge _confAppName a b
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
  , _ctxPreamble :: Text
  , _ctxSettings :: Settings
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

class UnTxt a where
  untxt :: Text -> Parser a

instance UnTxt String where
  untxt = return . unpack

instance UnTxt UTCTime where
  untxt s =
    untxt s >>= parseTimeM False defaultTimeLocale "%FT%T%z"

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

-- | Host name
--
type HostName = ByteString

-- | Metric group
--
type Group = Text

-- | Metric bucket
--
type Bucket = ByteString

-- | Metrics
--
-- These types are agnostic as to the metrics collector used, but are
-- largely inspired by the types provided by StatsD (see:
-- https://github.com/etsy/statsd/blob/master/docs/metric_types.md).
-- Each of these metrics can be sampled with some annotation.
--
data Metric =
  -- | A simple counter, set to 0 at each flush
    Counter Group Bucket Integer
  -- | A timing interval
  | Timer   Group Bucket Double
  -- | An arbitrary value that can be recorded
  | Gauge   Group Bucket Double
  -- | Counts unique occurrences of events between flushes.
  | Set     Group Bucket Double
    deriving (Show, Eq)

-- | Class of measurable instances.
--
class Measurable a where
  -- | Create a metric from some instance under some grouping
  -- A group typically uniquely identifies a host and an application.
  --
  measure :: Group -> a -> [Metric]

type SystemStat =
  ( Host
  , Memory
  , Load
  , [DiskIO]
  , [NetworkIO]
  )

instance Txt Metric where
  txt (Counter g b v) = sformat (stext % "." % stext % ":" % stext % "|c") g (txt b) (txt v)
  txt (Timer   g b v) = sformat (stext % "." % stext % ":" % stext % "|t") g (txt b) (txt v)
  txt (Gauge   g b v) = sformat (stext % "." % stext % ":" % stext % "|g") g (txt b) (txt v)
  txt (Set     g b v) = sformat (stext % "." % stext % ":" % stext % "|s") g (txt b) (txt v)

instance Measurable Host where
  measure gr Host{..} =
    [ Gauge gr "hostUptime" $ realToFrac hostUptime ]

instance Measurable Memory where
  measure gr Memory{..} =
    [ Gauge gr "memTotal"        $ fromIntegral memTotal
    , Gauge gr "memFreeFraction" $ fromIntegral memFree / fromIntegral memTotal
    , Gauge gr "memFree"         $ fromIntegral memFree
    , Gauge gr "memUsedFraction" $ fromIntegral memUsed / fromIntegral memTotal
    , Gauge gr "memUsed"         $ fromIntegral memUsed
    , Gauge gr "memCache"        $ fromIntegral memCache
    ]

instance Measurable Load where
  measure gr Load{..} =
    [ Gauge gr "load1"  load1
    , Gauge gr "load5"  load5
    , Gauge gr "load15" load15
    ]

instance Measurable DiskIO where
  measure gr DiskIO{..} =
    [ Gauge gr (diskName <> ".diskRead")  $ fromIntegral diskRead
    , Gauge gr (diskName <> ".diskWrite") $ fromIntegral diskWrite
    ]

instance Measurable a => Measurable [a] where
  measure gr = concatMap (measure gr)

instance Measurable NetworkIO where
  measure gr NetworkIO{..} =
    [ Gauge gr (ifaceIOName <> ".ifaceTX")         $ fromIntegral ifaceTX
    , Gauge gr (ifaceIOName <> ".ifaceRX")         $ fromIntegral ifaceRX
    , Gauge gr (ifaceIOName <> ".ifaceIPackets")   $ fromIntegral ifaceIPackets
    , Gauge gr (ifaceIOName <> ".ifaceOPackets")   $ fromIntegral ifaceOPackets
    , Gauge gr (ifaceIOName <> ".ifaceIErrors")    $ fromIntegral ifaceIErrors
    , Gauge gr (ifaceIOName <> ".ifaceCollisions") $ fromIntegral ifaceCollisions
    ]

instance Measurable SystemStat where
  measure gr (h, m, l, d, n) =
    measure gr h <>
    measure gr m <>
    measure gr l <>
    measure gr d <>
    measure gr n

instance MonadMask m => MonadMask (EitherT err m) where
  mask act = EitherT $ mask $ \restore ->
    runEitherT $ act (EitherT . restore . runEitherT)

  uninterruptibleMask act = EitherT $ uninterruptibleMask $ \restore ->
    runEitherT $ act (EitherT . restore . runEitherT)

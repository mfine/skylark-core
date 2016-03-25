-- |
-- Module:      Network.Skylark.Core.Conf
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Conf module for Skylark Core.

module Network.Skylark.Core.Conf
  ( bool
  , getConf
  , getDataFile
  , mandatory
  , parser
  , parseConf
  ) where

import Control.Lens
import Control.Monad.Logger
import Data.Aeson                   hiding (decode)
import Data.Default
import Data.Yaml                    hiding (Parser, decode)
import Network.Skylark.Core.Prelude
import Network.Skylark.Core.Types
import Options.Applicative
import System.Envy

--------------------------------------------------------------------------------
-- Configuration parsers

-- | Configuration file.
--
file :: Parser String
file =
  strOption
    $  long    "conf-file"
    <> short   'c'
    <> metavar "FILE"
    <> help    "Config file"

-- | Parse HTTP port.
--
port :: Parser Int
port =
  option auto
    $  long    "port"
    <> short   'p'
    <> metavar "PORT"
    <> help    "Port to listen on"

-- | Parse DDB port.
--
ddbLocalPort :: Parser Int
ddbLocalPort =
  option auto
    $  long    "ddb-local-port"
    <> metavar "PORT"
    <> help    "Localhost DDB port for testing (default none)"

-- | Parse connection timeout.
--
timeout :: Parser Int
timeout =
  option auto
    $  long    "timeout"
    <> short   't'
    <> metavar "TIMEOUT"
    <> help    "Timeout in seconds"

-- | Parse log level configuration.
--
logLevel :: Parser LogLevel
logLevel =
  fmap (toLogLevel . txt) $
    strOption
      $  long    "log-level"
      <> metavar "LEVEL"
      <> help    "Minimum level of logging" where
        toLogLevel "debug" = LevelDebug
        toLogLevel "info"  = LevelInfo
        toLogLevel "warn"  = LevelWarn
        toLogLevel "error" = LevelError
        toLogLevel s       = LevelOther s

-- | Application name.
--
appName :: Parser Text
appName =
  fmap txt $
    strOption
      $  long    "app-name"
      <> short   'n'
      <> metavar "APP-NAME"
      <> help    "Application name"

-- | Metrics collection.
--
metrics :: Parser Bool
metrics =
  switch
    $  long "metrics"
    <> help "Metrics Collection"

bool :: Parser Bool -> Parser (Maybe Bool)
bool =
  fmap $ \b ->
    if b then Just b else Nothing

-- | Configuration parser.
--
parseConf :: Parser Conf
parseConf = Conf    <$>
  optional file     <*>
  optional port     <*>
  optional timeout  <*>
  optional logLevel <*>
  optional appName  <*>
  bool metrics      <*>
  optional ddbLocalPort

-- | Produce a full command line options parser.
--
parser :: Parser a -> ParserInfo a
parser parse = info ( helper <*> parse ) fullDesc

-- | Execute parser and value from the parser.
--
getDataFile :: FromJSON a => String -> IO a
getDataFile f =
  decodeFileEither f >>= either throwIO return

-- | Return the full environmental configuration. Looks for the
-- configuration in three places (in order): a default, a
-- configuration file, command line options, and the environmental
-- configuration. Accepts the last non-Maybe value in each
--
getConf :: MonadConf a => Parser a -> (String -> IO String) -> IO a
getConf p fn = do
  e <- decode
  o <- execParser $ parser p
  f <- maybe (return Nothing) (fn >=> getDataFile) $ (^. confFile) $ def <> o <> fromMaybe mempty e
  return $ def <> fromMaybe mempty f <> o <> fromMaybe mempty e

-- | Check a mandatory configuration value. Return the value, or if
-- not present, throw a MandatoryConfException.
--
mandatory :: String -> Maybe a -> IO a
mandatory name =
  maybe (throwIO $ MandatoryConfException name) return

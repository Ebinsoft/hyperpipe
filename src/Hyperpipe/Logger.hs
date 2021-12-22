{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}
module Hyperpipe.Logger
  ( LogLevel(..)
  , Logger
  , HasLogger(..)
  , makeLogger
  , withLogger
  , logWithLevel
  , logDebug
  , logInfo
  , logWarn
  , logError
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Unagi
  (InChan, OutChan, newChan, readChan, writeChan)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Text.Printf (printf, PrintfArg(..))

data Logger = Logger
  { logChan  :: InChan LogMsg
  , minLevel :: LogLevel
  }

data LogLevel
  = DEBUG
  | INFO
  | WARN
  | ERROR
  deriving (Eq, Ord, Show)

instance PrintfArg LogLevel where
  formatArg DEBUG = formatArg "\x1b[37mDEBUG\x1b[0m"
  formatArg INFO  = formatArg "\x1b[36mINFO \x1b[0m"
  formatArg WARN  = formatArg "\x1b[33mWARN \x1b[0m"
  formatArg ERROR = formatArg "\x1b[31mERROR\x1b[0m"

type LogMsg = (UTCTime, LogLevel, String)

class (Monad m) => HasLogger m where
  getLogger :: m Logger

instance (Monad m) => HasLogger (ReaderT Logger m) where
  getLogger = ask

makeLogger :: LogLevel -> IO Logger
makeLogger lvl = do
  (inChn, outChn) <- newChan
  forkIO $ runLogWorker outChn
  return $ Logger inChn lvl

runLogWorker :: OutChan LogMsg -> IO ()
runLogWorker chn = loop
 where
  loop = do
    (time, lvl, msg) <- readChan chn
    let timeS = formatTime defaultTimeLocale "%F %X" time
    let log = printf "[%s] %s | %s" timeS lvl msg
    putStrLn log
    loop

withLogger :: Logger -> ReaderT Logger m () -> m ()
withLogger l m = runReaderT m l

logWithLevel :: (HasLogger m, MonadIO m) => LogLevel -> String -> m ()
logWithLevel lvl msg = do
  Logger {..} <- getLogger
  time        <- liftIO getCurrentTime
  when (lvl >= minLevel) (liftIO $ writeChan logChan (time, lvl, msg))

logDebug :: (HasLogger m, MonadIO m) => String -> m ()
logDebug = logWithLevel DEBUG

logInfo :: (HasLogger m, MonadIO m) => String -> m ()
logInfo = logWithLevel INFO

logWarn :: (HasLogger m, MonadIO m) => String -> m ()
logWarn = logWithLevel WARN

logError :: (HasLogger m, MonadIO m) => String -> m ()
logError = logWithLevel ERROR

{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Module: Logger
--
-- All logging-related types and functions are kept here.
--
-- The `makeLogger` function should only be called once at the beginning of the
-- program's lifetime, and the resulting `Logger` should be passed along to any
-- threads that need to write log messages.  A worker thread is created when
-- `makeLogger` is called, and the returned `Logger` represents a connection to
-- that thread.  When messages are logged using a `Logger`, they are pushed onto
-- a queue, which the worker thread reads from one at a time so that messages
-- can be logged asynchronously.
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

-- | Type representing the connection to a logging worker thread, with which log
-- messages can be written.
data Logger = Logger
  { logChan  :: InChan LogMsg
  , minLevel :: LogLevel
  }

-- | Urgency level for a log message, ordered from lowest to highest priority.
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

-- | Alias for tuple of all data associated with a log message.
type LogMsg = (UTCTime, LogLevel, String)

-- | mtl-style typeclass which gives a monad the ability to send log messages.
class (Monad m) => HasLogger m where
  getLogger :: m Logger

instance (Monad m) => HasLogger (ReaderT Logger m) where
  getLogger = ask

-- | Create a worker thread and return a `Logger` for sending log messages to
-- that thread.
makeLogger :: LogLevel -> IO Logger
makeLogger lvl = do
  (inChn, outChn) <- newChan
  forkIO $ runLogWorker outChn
  return $ Logger inChn lvl

-- | Reads a message from the queue, formats it, and prints to stdout in an
-- infinite loop.
runLogWorker :: OutChan LogMsg -> IO ()
runLogWorker chn = loop
 where
  loop = do
    (time, lvl, msg) <- readChan chn
    let timeS = formatTime defaultTimeLocale "%F %X" time
    let log = printf "[%s] %s | %s" timeS lvl msg
    putStrLn log
    loop

-- | Helper function for logging messages outside of a `HasLogger` monad.
withLogger :: Logger -> ReaderT Logger m () -> m ()
withLogger l m = runReaderT m l

-- | Send a log message with a given urgency level.
logWithLevel :: (HasLogger m, MonadIO m) => LogLevel -> String -> m ()
logWithLevel lvl msg = do
  Logger {..} <- getLogger
  time        <- liftIO getCurrentTime
  when (lvl >= minLevel) (liftIO $ writeChan logChan (time, lvl, msg))

-- | Send a `DEBUG`-level log message.
logDebug :: (HasLogger m, MonadIO m) => String -> m ()
logDebug = logWithLevel DEBUG

-- | Send a `INFO`-level log message.
logInfo :: (HasLogger m, MonadIO m) => String -> m ()
logInfo = logWithLevel INFO

-- | Send a `WARN`-level log message.
logWarn :: (HasLogger m, MonadIO m) => String -> m ()
logWarn = logWithLevel WARN

-- | Send a `ERROR`-level log message.
logError :: (HasLogger m, MonadIO m) => String -> m ()
logError = logWithLevel ERROR

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
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.IO.Class (MonadIO, liftIO)

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

type LogMsg = (LogLevel, String)

class (Monad m) => HasLogger m where
  getLogger :: m Logger

instance (Monad m) => HasLogger (ReaderT Logger m) where
  getLogger = ask

makeLogger :: LogLevel -> IO Logger
makeLogger lvl = do
  (inChn, outChn) <- newChan
  forkIO $ runLogWorker outChn
  return $ Logger inChn lvl

prefix :: LogLevel -> String
prefix DEBUG = "[DEBUG] "
prefix INFO  = "[INFO]  "
prefix WARN  = "[WARN]  "
prefix ERROR = "[ERROR] "

runLogWorker :: OutChan LogMsg -> IO ()
runLogWorker chn = loop
 where
  loop = do
    (lvl, msg) <- readChan chn
    putStrLn $ prefix lvl ++ msg
    loop
  
withLogger :: Logger -> ReaderT Logger m () -> m ()
withLogger l m = runReaderT m l
  
logWithLevel :: (HasLogger m, MonadIO m) => LogLevel -> String -> m ()
logWithLevel lvl msg = do
  Logger {..} <- getLogger
  when (lvl >= minLevel) (liftIO $ writeChan logChan (lvl, msg))

logDebug :: (HasLogger m, MonadIO m) => String -> m ()
logDebug = logWithLevel DEBUG

logInfo :: (HasLogger m, MonadIO m) => String -> m ()
logInfo = logWithLevel INFO

logWarn :: (HasLogger m, MonadIO m) => String -> m ()
logWarn = logWithLevel WARN

logError :: (HasLogger m, MonadIO m) => String -> m ()
logError = logWithLevel ERROR

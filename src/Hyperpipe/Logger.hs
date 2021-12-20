{-# LANGUAGE FlexibleContexts #-}
module Hyperpipe.Logger where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Unagi
  (InChan, OutChan, newChan, readChan, writeChan)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, ask, liftIO)

type LogHandle = InChan LogMsg

data LogLevel = DEBUG | INFO | WARN | ERROR
type LogMsg = (LogLevel, String)

class (MonadReader LogHandle m, MonadIO m) => MonadLog m where
type LogT = ReaderT LogHandle

makeLogger :: IO LogHandle
makeLogger = do
  (inChn, outChn) <- newChan
  forkIO $ logWorker outChn
  return inChn

prefix :: LogLevel -> String
prefix DEBUG = "[DEBUG] "
prefix INFO  = "[INFO]  "
prefix WARN  = "[WARN]  "
prefix ERROR = "[ERROR] "

logWorker :: OutChan LogMsg -> IO ()
logWorker chn = loop
 where
  loop = do
    (lvl, msg) <- readChan chn
    putStrLn $ prefix lvl ++ msg
    loop

log :: (MonadLog m) => LogLevel -> String -> m ()
log lvl msg = do
  chn <- ask
  liftIO $ writeChan chn (lvl, msg)

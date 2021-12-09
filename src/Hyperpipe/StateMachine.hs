{-# LANGUAGE FlexibleContexts #-}

-- | Module: StateMachine
--
-- This module contains the entrypoint for the program, and it is where most of
-- the effectful, IO-centric operations take place.
--
-- We call this the `StateMachine` because we are modeling the entire program's
-- behavior as an abstract state machine where each "state" is a set of
-- `Endpoint`s (see the documentation of the "StateModel" module).
--
-- This ASM "transitions" between states by acting on `Instruction`s, which will
-- create or destroy the worker threads which send/receive traffic on a network
-- interface.
module Hyperpipe.StateMachine where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)

import Control.Concurrent.Chan.Unagi.Bounded
  (InChan, OutChan, readChan, writeChan)

import Control.Monad (forever, when)
import Control.Monad.Reader
  (MonadIO, MonadReader, ReaderT, ask, asks, lift, runReaderT)
import Control.Monad.State.Strict (StateT(..), get, liftIO, put)
import Data.Binary (encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Network.Pcap (PcapHandle, nextBS, openLive, sendPacketBS)

import Hyperpipe.EthFrame
import Hyperpipe.StateModel

-- | Custom newtype wrapper for using `Endpoint` as the key in an ordered `Map`
-- (orders by interface name).
newtype Key = Key Endpoint
  deriving (Show, Eq)

instance Ord Key where
  compare (Key e) (Key f) = comparing ifaceName e f

type Elem = Either ByteString EthFrame
type Env = (InChan Elem, OutChan Elem, Map Key ThreadId)

-- | Read-only configuration options for the `StateMachine` and all of its
-- `Worker` threads
data Settings = Settings
  { debugMode  :: Bool -- ^ when true, packet info is printed while running
  , bufTimeout :: Int  -- ^ packet buffer timeout for pcap handles
  }

type StateMachine a = ReaderT Settings (StateT Env IO) a
type Worker a = ReaderT Settings IO a

-- | Bootstrap the `StateMachine` with a `StateModel` the target program
-- configuration.
runWithModel :: StateModel -> StateMachine ()
runWithModel model = do
  let instructions = stepsBetween (StateModel []) model
  mapM_ interpret instructions
  forever $ liftIO (threadDelay 100000000)

-- | Execute an `Instruction` as an effect in our `StateMachine`
interpret :: Instruction -> StateMachine ()
interpret (EnableEndpoint  e) = createWorker e
interpret (DisableEndpoint e) = destroyWorker e

-- | Start a thread to transfer traffic to or from a network interface. If a
-- thread for the same interface already exists, kill it.
createWorker :: Endpoint -> StateMachine ()
createWorker ep = do
  (inChn, outChn, wmap) <- get
  -- kill existing thread if it exists
  liftIO $ maybe (pure ()) killThread (M.lookup (Key ep) wmap)

  -- create pcap handle for interface
  debugStrLn $ "Creating worker for " ++ show (ifaceName ep)
  settings <- ask
  let (IfaceName name) = ifaceName ep
  timeout <- asks (fromIntegral . bufTimeout)
  hnd     <- liftIO $ openLive name 65535 True timeout
  let f = runOps $ frameOps ep
  let
    worker = case trafficDir ep of
      Input  -> forever $ inputWorker hnd f inChn
      Output -> forever $ outputWorker hnd f outChn

  -- run thread for new worker
  tid <- liftIO $ forkIO (runReaderT worker settings)
  put (inChn, outChn, M.insert (Key ep) tid wmap)

-- | Kill the thread transferring traffic to or from the given network interface
-- (if it exists).
destroyWorker :: Endpoint -> StateMachine ()
destroyWorker ep = do
  (inChn, outChn, wmap) <- get
  case M.lookup (Key ep) wmap of
    Nothing  -> return ()
    Just tid -> do
      liftIO (killThread tid)
      put (inChn, outChn, M.delete (Key ep) wmap)

-- | Convert a list of `FrameOp`s into a single function over `EthFrame`s doing
-- all of the ops.
runOps :: [FrameOp] -> (EthFrame -> EthFrame)
runOps []       = id
runOps (o : os) = runOps os . opToFunc o
 where
  opToFunc (SetVLAN vt) = setVlan vt
  opToFunc StripVLAN    = stripVlan

-- | Pull packet from interface handle, apply function, and put into `Chan`.
inputWorker :: PcapHandle -> (EthFrame -> EthFrame) -> InChan Elem -> Worker ()
inputWorker hnd f chn = do
  (_, bs) <- liftIO $ nextBS hnd
  let bs' = BL.fromStrict bs
  if BL.length bs' == 0
    then return ()  -- ignore empty frames (probably just a timeout)
    else do
      debugStr $ "Received (" ++ show (BL.length bs') ++ " bytes)\t"
      elem <- case parseFrame bs' of
        Left err -> do
          debugStrLn $ "failed to parse: " ++ err
          return (Left bs')
        Right ef -> do
          debugStrLn $ showFrameInfo ef
          return (Right $ f ef)
      liftIO $ writeChan chn elem

-- | Pull packet from `Chan`, apply function, and write to network interface.
outputWorker :: PcapHandle -> (EthFrame -> EthFrame) -> OutChan Elem -> Worker ()
outputWorker hnd f chn = do
  elem <- liftIO $ readChan chn
  let
    bs = case elem of
      Left  bs' -> bs'
      Right ef  -> encode (f ef)
  liftIO $ sendPacketBS hnd (BL.toStrict bs)
  debugStrLn $ "Sent     (" ++ show (BL.length bs) ++ " bytes)"

-- | Print a string only when debug mode is enabled in `Settings`.
debugStr :: (MonadReader Settings m, MonadIO m) => String -> m ()
debugStr s = do
  isDebug <- asks debugMode
  when isDebug (liftIO $ putStr s)

-- | Identical to `debugStr` but appends a newline to the end of the string.
debugStrLn :: (MonadReader Settings m, MonadIO m) => String -> m ()
debugStrLn s = debugStr $ s ++ "\n"

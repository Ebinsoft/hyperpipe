{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Control.Concurrent.MVar (readMVar)
import Control.Monad (forever, when)
import Control.Monad.Reader (ReaderT, ask, asks, lift, runReaderT)
import Control.Monad.State.Strict (StateT(..), get, liftIO, put)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.Serialize (decode, encode)
import Network.Pcap (PcapHandle, nextBS, openLive, sendPacketBS)

import Hyperpipe.EthFrame
import Hyperpipe.Logger
import Hyperpipe.StateModel
import Hyperpipe.UsageMonitor

-- | Custom newtype wrapper for using `Endpoint` as the key in an ordered `Map`
-- (orders by interface name).
newtype Key = Key Endpoint
  deriving (Show, Eq)

instance Ord Key where
  compare (Key e) (Key f) = comparing ifaceName e f

type Elem = Either ByteString EthFrame

data Env = Env
  { bufTimeout :: Int          -- ^ packet buffer timeout for pcap handles
  , queueIn    :: InChan Elem  -- ^ input for the traffic queue
  , queueOut   :: OutChan Elem -- ^ output for the traffic queue
  , logger     :: Logger
  }

type WorkerMap = Map Key ThreadId

type StateMachine = ReaderT Env (StateT WorkerMap IO)
type Worker = ReaderT Env IO

instance (Monad m) => HasLogger (ReaderT Env m) where
  getLogger = asks logger

-- | Bootstrap the `StateMachine` with a `StateModel` representing the target
-- program configuration.
runWithModel :: StateModel -> StateMachine ()
runWithModel model = do
  let instructions = stepsBetween (StateModel []) model
  mapM_ interpret instructions -- spins off worker threads

  -- print throughput every second in a loop
  forever $ do
    monitor <- asks (monitorVar . logger)
    stuff   <- liftIO $ readMVar monitor >>= getThroughput
    liftIO $ print stuff
    liftIO $ threadDelay 1000000


-- | Execute an `Instruction` as an effect in our `StateMachine`
interpret :: Instruction -> StateMachine ()
interpret (EnableEndpoint  e) = createWorker e
interpret (DisableEndpoint e) = destroyWorker e

-- | Start a thread to transfer traffic to or from a network interface. If a
-- thread for the same interface already exists, kill it.
createWorker :: Endpoint -> StateMachine ()
createWorker ep = do
  wmap <- get
  -- kill existing thread if it exists
  liftIO $ maybe (pure ()) killThread (M.lookup (Key ep) wmap)

  -- create pcap handle for interface
  let (IfaceName name) = ifaceName ep
  timeout <- asks (fromIntegral . bufTimeout)
  hnd     <- liftIO $ openLive name 65535 True timeout
  let f = runOps $ frameOps ep
  let
    worker = case trafficDir ep of
      Input  -> inputWorker name hnd f
      Output -> outputWorker name hnd f

  -- run thread for new worker
  logInfo $ "Spawning worker thread for " ++ name
  env <- ask
  tid <- liftIO $ forkIO (runReaderT worker env)
  put $ M.insert (Key ep) tid wmap

-- | Kill the thread transferring traffic to or from the given network interface
-- (if it exists).
destroyWorker :: Endpoint -> StateMachine ()
destroyWorker ep = do
  wmap <- get
  case M.lookup (Key ep) wmap of
    Nothing  -> return ()
    Just tid -> do
      liftIO (killThread tid)
      put $ M.delete (Key ep) wmap

-- | Convert a list of `FrameOp`s into a single function over `EthFrame`s doing
-- all of the ops.
runOps :: [FrameOp] -> (EthFrame -> EthFrame)
runOps []       = id
runOps (o : os) = runOps os . opToFunc o
 where
  opToFunc (AddVLAN vt) = addVlan vt
  opToFunc StripVLAN    = stripVlan

-- | Pull packet from interface handle, apply function, and put into channel in
-- an infinite loop
inputWorker :: String -> PcapHandle -> (EthFrame -> EthFrame) -> Worker ()
inputWorker iface hnd f = forever $ do
  (_, bs) <- liftIO $ nextBS hnd
  if BS.length bs == 0
    then return ()  -- ignore empty frames (probably just a timeout)
    else do
      logMetric iface (BS.length bs)
      elem <- case decode bs of
        Left err -> do
          logError $ "Failed to parse packet: " ++ err
          return (Left bs)
        Right ef -> do
          return (Right $ f ef)
      chn <- asks queueIn
      liftIO $ writeChan chn elem

-- | Pull packet from channel, apply function, and write to network interface in
-- an infinite loop
outputWorker :: String -> PcapHandle -> (EthFrame -> EthFrame) -> Worker ()
outputWorker iface hnd f = forever $ do
  chn  <- asks queueOut
  elem <- liftIO $ readChan chn
  let
    bs = case elem of
      Left  bs' -> bs'
      Right ef  -> encode (f ef)
  liftIO $ sendPacketBS hnd bs
  logMetric iface (BS.length bs)


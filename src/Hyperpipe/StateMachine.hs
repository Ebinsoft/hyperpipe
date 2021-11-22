module Hyperpipe.StateMachine where

import Control.Concurrent
  (Chan, ThreadId, forkIO, killThread, readChan, threadDelay, writeChan)
import Control.Monad (forever)
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
type Env = (Chan Elem, Map Key ThreadId)

type StateMachine a = StateT Env IO a


mainLoop :: StateMachine ()
mainLoop = undefined

runWithConfig :: StateModel -> StateMachine ()
runWithConfig model = do
  let instructions = stepsBetween (StateModel []) model
  mapM_ interpret instructions
  forever $ liftIO (threadDelay 100000000)

-- | Execute an `Instruction` as an effect in our `StateMachine`
interpret :: Instruction -> StateMachine ()
interpret (EnableEndpoint ep) = do
  env   <- get
  wmap' <- liftIO $ createWorker ep env
  put env
interpret (DisableEndpoint ep) = do
  env   <- get
  wmap' <- liftIO $ destroyWorker ep env
  put env

-- | Start a thread to transfer traffic to or from a network interface. If a
-- thread for the same interface already exists, kill it.
createWorker :: Endpoint -> Env -> IO Env
createWorker ep (chn, wmap) = do
  maybe (pure ()) killThread (M.lookup (Key ep) wmap)
  tid <- forkIO $ runWorker ep chn
  return (chn, M.insert (Key ep) tid wmap)

-- | Kill the thread transferring traffic to or from the given network interface
-- (if it exists).
destroyWorker :: Endpoint -> Env -> IO Env
destroyWorker ep env@(chn, wmap) = case M.lookup (Key ep) wmap of
  Nothing  -> return env
  Just tid -> killThread tid >> return (chn, M.delete (Key ep) wmap)

-- | Infinite loop transferring packets between `Chan` and network interface
runWorker :: Endpoint -> Chan Elem -> IO ()
runWorker ep chn = do
  let (IfaceName name) = ifaceName ep
  hnd <- openLive name 10000 True 0
  let f = runOps $ frameOps ep
  case trafficDir ep of
    Input  -> forever $ runInput hnd f chn
    Output -> forever $ runOutput hnd f chn

-- | Convert a list of `FrameOp`s into a single function over `EthFrame`s doing
-- all of the ops.
runOps :: [FrameOp] -> (EthFrame -> EthFrame)
runOps []       = id
runOps (o : os) = runOps os . opToFunc o
 where
  opToFunc (SetVLAN vt) = setVlan vt
  opToFunc StripVLAN    = stripVlan

-- | Pull packet from interface handle, apply function, and put into `Chan`.
runInput :: PcapHandle -> (EthFrame -> EthFrame) -> Chan Elem -> IO ()
runInput hnd f chn = do
  (_, bs) <- nextBS hnd
  let bs' = BL.fromStrict bs
  elem <- case parseFrame bs' of
    Left  err -> putStrLn err >> return (Left bs')
    Right ef  -> return (Right $ f ef)
  writeChan chn elem

-- | Pull packet from `Chan`, apply function, and write to network interface.
runOutput :: PcapHandle -> (EthFrame -> EthFrame) -> Chan Elem -> IO ()
runOutput hnd f chn = do
  elem <- readChan chn
  let
    bs = case elem of
      Left  bs' -> bs'
      Right ef  -> encode (f ef)
  sendPacketBS hnd (BL.toStrict bs)

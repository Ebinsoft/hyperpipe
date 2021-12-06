module Hyperpipe.StateMachine where

import Control.Concurrent
  (Chan, ThreadId, forkIO, killThread, readChan, threadDelay, writeChan)
import Control.Monad (forever, when)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
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

-- | Read-only configuration options for the `StateMachine` and all of its
-- `Worker` threads
newtype Settings = Settings
  { debugMode :: Bool
  }

type StateMachine a = ReaderT Settings (StateT Env IO) a
type Worker a = ReaderT Settings IO a


mainLoop :: StateMachine ()
mainLoop = undefined

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
  (chn, wmap) <- get
  -- kill existing thread if it exists
  liftIO $ maybe (pure ()) killThread (M.lookup (Key ep) wmap)
  -- run new thread
  settings <- ask
  tid      <- liftIO $ forkIO (runReaderT (worker ep chn) settings)
  put (chn, M.insert (Key ep) tid wmap)

-- | Kill the thread transferring traffic to or from the given network interface
-- (if it exists).
destroyWorker :: Endpoint -> StateMachine ()
destroyWorker ep = do
  (chn, wmap) <- get
  case M.lookup (Key ep) wmap of
    Nothing  -> return ()
    Just tid -> do
      liftIO (killThread tid)
      put (chn, M.delete (Key ep) wmap)

-- | Infinite loop transferring packets between `Chan` and network interface
worker :: Endpoint -> Chan Elem -> Worker ()
worker ep chn = do
  let (IfaceName name) = ifaceName ep
  hnd <- liftIO $ openLive name 65535 True 1000
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
runInput :: PcapHandle -> (EthFrame -> EthFrame) -> Chan Elem -> Worker ()
runInput hnd f chn = do
  (_, bs) <- liftIO $ nextBS hnd
  let bs' = BL.fromStrict bs
  settings <- ask
  if BL.length bs' == 0
    then return ()
    else do
      when (debugMode settings) (liftIO $ debugBS bs')
      elem <- case parseFrame bs' of
        Left err -> do
          when (debugMode settings) (liftIO $ putStr "failed to parse: ")
          liftIO $ putStrLn err
          return (Left bs')
        Right ef -> do
          when (debugMode settings) (liftIO $ putStrLn "parsing successful")
          return (Right $ f ef)
      liftIO $ writeChan chn elem
 where
  debugBS bs =
    putStr $ "Received packet (" ++ show (BL.length bs) ++ " bytes)\t"

-- | Pull packet from `Chan`, apply function, and write to network interface.
runOutput :: PcapHandle -> (EthFrame -> EthFrame) -> Chan Elem -> Worker ()
runOutput hnd f chn = do
  elem <- liftIO $ readChan chn
  let
    bs = case elem of
      Left  bs' -> bs'
      Right ef  -> encode (f ef)
  liftIO $ sendPacketBS hnd (BL.toStrict bs)

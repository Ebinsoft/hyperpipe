module Hyperpipe.StateMachine where

import Control.Concurrent
  (Chan, ThreadId, forkIO, killThread, readChan, writeChan)
import Control.Monad.State.Strict
import Data.Binary (encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as M
import Network.Pcap (PcapHandle, nextBS, openLive, sendPacketBS)

import Hyperpipe.EthFrame
import Hyperpipe.StateModel

type Elem = Either ByteString EthFrame
type Env = (Chan Elem, Map IfaceName ThreadId)

type StateMachine a = StateT Env IO a


mainLoop :: StateMachine ()
mainLoop = undefined

-- | Execute an `Instruction` as an effect in our `StateMachine`
interpret :: Instruction -> StateMachine ()
interpret (EnableEndpoint  ep) = enableEndpoint ep
interpret (DisableEndpoint ep) = disableEndpoint ep

-- | Create a thread to transfer network traffic from or to a network
-- interface. If a thread for the same interface already exists, kill it before
-- spinning up the new one.
enableEndpoint :: Endpoint -> StateMachine ()
enableEndpoint ep = do
  let name = ifaceName ep
  (chn, tmap) <- get
  -- if we already have a thread running on an interface, kill it
  liftIO $ maybe (pure ()) killThread (M.lookup name tmap)

  tid <- liftIO (forkIO $ runEndpoint ep chn)
  let tmap' = M.insert name tid tmap
  put (chn, tmap')

-- | Kill the thread transferring traffic to or from the given network interface
-- (if it exists).
disableEndpoint :: Endpoint -> StateMachine ()
disableEndpoint ep = do
  let name = ifaceName ep
  (chn, tmap) <- get
  case M.lookup name tmap of
    Nothing  -> pure ()
    Just tid -> do
      liftIO $ killThread tid
      put (chn, M.delete name tmap)

-- | Infinite loop transferring packets between `Chan` and network interface
runEndpoint :: Endpoint -> Chan Elem -> IO ()
runEndpoint ep chn = do
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

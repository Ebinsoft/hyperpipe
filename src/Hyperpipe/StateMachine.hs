module Hyperpipe.StateMachine where

import Control.Concurrent (Chan, ThreadId, forkIO, killThread)
import Control.Monad.State.Strict
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as M

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


runEndpoint :: Endpoint -> Chan Elem -> IO ()
runEndpoint = undefined

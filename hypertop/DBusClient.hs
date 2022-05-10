{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module: DBusClient
--
-- Contains all D-Bus interfacing logic (calling methods, parsing reponses, etc)
-- as well as the client that will continuously send throughput information to
-- the Brick app.
module DBusClient where

import Brick.BChan (BChan, writeBChan)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import DBus
import DBus.Client
import Data.Bifunctor (bimap)
import Data.Int (Int32)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as S
import Data.Time.Clock (UTCTime, getCurrentTime)

import Types

-- | Maximum number of data points to keep in history
maxHistoryLen :: Int
maxHistoryLen = 60

-- | Fetches throughput information from hyperpipe-daemon via dbus, parses it,
-- and gives it to the Brick app as an event, every second in an infinite loop.
runClient :: BChan Event -> Client -> IO ()
runClient bchan client = forever $ do
  reply <- call_
    client
    (methodCall "/com/hyperpipe/metrics" "com.hyperpipe.metrics" "throughput")
      { methodCallDestination = Just "com.hyperpipe"
      }
  let info = parseThroughputResponse (head $ methodReturnBody reply)
  time <- getCurrentTime
  writeBChan bchan (UpdateInfo time info)
  threadDelay 1000000

-- | Constructs an initial `State` containing each interface present in the
-- response from a call to the "interfaces" method.
parseIfacesResponse :: Variant -> State
parseIfacesResponse var =
  case fromVariant var :: Maybe [(String, String, String)] of
    Just ifaces -> foldl addIface M.empty ifaces
    Nothing     -> error "Malformed dbus message body (interfaces)"
 where
  addIface st (name, dir, vlan) =
    let info = IfaceInfo vlan (parseDir dir) S.empty in M.insert name info st
  parseDir "Input" = Input
  parseDir "Output" = Output
  parseDir _ = error "Malformed dbus message body (interfaces - direction)"

-- | Constructs a data structure containing the most recent throughput
-- information for each interface from the response to a call to the
-- "throughput" method.
parseThroughputResponse :: Variant -> ThroughputInfo
parseThroughputResponse var =
  case fromVariant var :: Maybe (Map String (Int32, Int32)) of
    Just m  -> bimap fromIntegral fromIntegral <$> m
    Nothing -> error "Malformed dbus message body (throughput)"

-- | Applies the new data contained in a `ThroughputInfo` map to an existing
-- `State`, distributing each data point to the correct interface.
updateState :: State -> UTCTime -> ThroughputInfo -> State
updateState st time tpMap = foldl updateIface st (M.toList tpMap)
 where
  updateIface st (iface, (pps, bps)) = case M.lookup iface st of
    Just info@IfaceInfo {..} ->
      M.insert iface (addToHistory info (time, pps, bps)) st
    Nothing -> st
  -- add data to history, dropping oldest element off if exceeding max length
  addToHistory info@IfaceInfo {..} elem =
    let
      history = if S.length usageHistory > maxHistoryLen
        then S.drop 1 usageHistory
        else usageHistory
    in info { usageHistory = history |> elem }

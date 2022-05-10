{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Sequence ((|>))
import qualified Data.Sequence as S
import Data.Time.Clock (UTCTime, getCurrentTime)

import Types

runClient :: BChan Event -> Client -> IO ()
runClient bchan client = forever $ do
  reply <- call_
    client
    (methodCall "/com/hyperpipe/metrics" "com.hyperpipe.metrics" "throughput")
      { methodCallDestination = Just "com.hyperpipe"
      }
  time <- getCurrentTime
  let update = parseThroughputResponse (head $ methodReturnBody reply)
  writeBChan bchan (UpdateInfo time update)
  threadDelay 1000000

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

parseThroughputResponse :: Variant -> Map String (Packets, Bytes)
parseThroughputResponse var =
  case fromVariant var :: Maybe (Map String (Int32, Int32)) of
    Just m  -> bimap fromIntegral fromIntegral <$> m
    Nothing -> error "Malformed dbus message body (throughput)"

updateState :: State -> UTCTime -> Map String (Packets, Bytes) -> State
updateState st time tpMap = foldl updateIface st (M.toList tpMap)
 where
  updateIface st (iface, (pps, bps)) = case M.lookup iface st of
    Just info@IfaceInfo {..} -> M.insert
      iface
      info { usageHistory = usageHistory |> (time, pps, bps) }
      st
    Nothing -> st

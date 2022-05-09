{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import DBus
import DBus.Client
import Data.Bifunctor (bimap)
import Data.Int (Int32)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import Data.Time.Clock (UTCTime, getCurrentTime)

type Packets = Int
type Bytes = Int

data IfaceInfo = IfaceInfo
  { vlanSetting  :: String
  , ifaceDir     :: String
  , usageHistory :: Seq (UTCTime, Packets, Bytes)
  }
  deriving Show

type State = Map String IfaceInfo


parseIfacesResponse :: Variant -> State
parseIfacesResponse var =
  case fromVariant var :: Maybe [(String, String, String)] of
    Just ifaces -> foldl addIface M.empty ifaces
    Nothing     -> error "Malformed dbus message body (interfaces)"
 where
  addIface st (name, dir, vlan) =
    let info = IfaceInfo vlan dir S.empty in M.insert name info st


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

main :: IO ()
main = do
  client      <- connectSession

  ifacesReply <- call_
    client
    (methodCall "/com/hyperpipe/metrics" "com.hyperpipe.metrics" "interfaces")
      { methodCallDestination = Just "com.hyperpipe"
      }

  let state = parseIfacesResponse (head $ methodReturnBody ifacesReply)

  tpReply <- call_
    client
    (methodCall "/com/hyperpipe/metrics" "com.hyperpipe.metrics" "throughput")
      { methodCallDestination = Just "com.hyperpipe"
      }

  let update = parseThroughputResponse (head $ methodReturnBody tpReply)

  time <- getCurrentTime
  print $ updateState state time update

  


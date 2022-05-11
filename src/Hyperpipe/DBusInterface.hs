{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Hyperpipe.DBusInterface where

import Control.Concurrent (readMVar)
import Control.Monad (when)
import DBus.Client
import Data.Bifunctor (bimap)
import Data.Int (Int32)
import Data.Map (Map)
import qualified Data.Map as M
import System.Exit (exitFailure)

import Hyperpipe.EthFrame
import Hyperpipe.Logger
import Hyperpipe.StateModel
import Hyperpipe.UsageMonitor

busName = "com.hyperpipe"

-- | Handler for the "/interfaces" method.  Responds with the list of
-- interfaces that are included in the active Hyperpipe configuration.
-- Each interface is encoded as a 3-tuple of (device, direction, vlan)
onInterfaces :: StateModel -> IO [(String, String, String)]
onInterfaces (StateModel eps) = return (convert <$> eps)
 where
  convert Endpoint {..} =
    let
      IfaceName name = ifaceName
      vlanSet        = case frameOps of -- for now only consider first operation
        []               -> ""
        (AddVLAN vt : _) -> show (vlan vt)
        (StripVLAN  : _) -> "remove"
    in (name, show trafficDir, vlanSet)

-- | Handler for the "/throughput" method.  Responds with a mapping
-- from interface name to 2-tuple of (packets, bytes) that were
-- received over the last second.
onThroughput :: Logger -> IO (Map String (Int32, Int32))
onThroughput Logger {..} = do
  tpMap <- readMVar monitorVar >>= getThroughput
  return $ bimap fromIntegral fromIntegral <$> tpMap

-- | Creates and exports a D-Bus interface with methods for querying
-- the application's configuration and network utilization.
-- 
-- Propogates exceptions from the dbus library functions and throws a
-- generic error if it cannot acquire its bus name.
initDBusInterface :: StateModel -> Logger -> IO ()
initDBusInterface model logger = do
  client    <- connectSystem

  reqResult <- requestName client busName []
  when (reqResult /= NamePrimaryOwner) $ do
    errorWithoutStackTrace
      "D-Bus name conflict (is another instance of hyperpipe already running?)"

  export
    client
    "/com/hyperpipe/metrics"
    defaultInterface
      { interfaceName    = "com.hyperpipe.metrics"
      , interfaceMethods =
        [ autoMethod "interfaces" (onInterfaces model)
        , autoMethod "throughput" (onThroughput logger)
        ]
      }




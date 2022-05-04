-- | Module: UsageMonitor
--
-- This module is centered around the `UsageMonitor` type which tracks network
-- usage metrics (currently bitrate and packet throughput) for each interface in
-- the pipeline.
module Hyperpipe.UsageMonitor where

import Data.Map (Map(..))
import qualified Data.Map as M
import Data.Sequence (Seq(..), ViewR(..), (|>))
import qualified Data.Sequence as S
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)

type IfaceName = String
type PacketSize = Int

type UsageMonitor = Map IfaceName (Seq (UTCTime, Int))

-- | Remove all data points from over 1s ago.
pruneOldMetrics :: UsageMonitor -> IO UsageMonitor
pruneOldMetrics monitor = do
  time <- getCurrentTime
  -- seq should always be ordered old to new so only need to drop from left size
  return $ S.dropWhileL (\i -> diffUTCTime time (fst i) > 1.0) <$> monitor

-- | Create a data point for a given network interface, using the timestamp and
-- size of one packet sent/received on that interface.
addMetric :: UsageMonitor -> IfaceName -> (UTCTime, Int) -> IO UsageMonitor
addMetric monitor iface (ts, size) = do
  let metrics = M.findWithDefault Empty iface monitor
  pruneOldMetrics (M.insert iface (insertSorted (ts, size) metrics) monitor)
 where
  insertSorted (newTime, newSize) elems = case S.viewr elems of
    EmptyR                       -> elems |> (newTime, newSize)
    (xs :> (lastTime, lastSize)) -> if newTime < lastTime
      then insertSorted (newTime, newSize) xs |> (lastTime, lastSize)
      else elems |> (newTime, newSize)

-- | Get the current throughput information for all interfaces as a map from
-- interface name to a tuple containing the number of packets and total number
-- of bytes to pass through that interface within the last second.
getThroughput :: UsageMonitor -> IO (Map IfaceName (Int, Int))
getThroughput tt = do
  tt' <- pruneOldMetrics tt
  return $ (\ms -> (length ms, sum (snd <$> ms))) <$> tt'

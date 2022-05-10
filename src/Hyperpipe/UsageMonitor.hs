-- | Module: UsageMonitor
--
-- This module is centered around the `UsageMonitor` type which tracks network
-- usage metrics (currently bitrate and packet throughput) for each interface in
-- the pipeline.
module Hyperpipe.UsageMonitor where

import Data.Map (Map(..))
import qualified Data.Map as M
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as S
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)

import Hyperpipe.StateModel

-- | Data structure keeping track of recent network usage info for
-- many interfaces.
type UsageMonitor = Map IfaceName (Seq (UTCTime, Int))

-- | Remove all data points from over 1s ago.
pruneOldMetrics :: UsageMonitor -> IO UsageMonitor
pruneOldMetrics monitor = do
  time <- getCurrentTime
  -- seq will always be ordered old to new so only need to drop from left side
  return $ S.dropWhileL (\i -> diffUTCTime time (fst i) > 1.0) <$> monitor

-- | Create a data point for a given network interface, using the timestamp and
-- size of one packet sent/received on that interface.
addMetric :: UsageMonitor -> IfaceName -> (UTCTime, Int) -> IO UsageMonitor
addMetric mon iface metric = pruneOldMetrics $ M.alter addMetric' iface mon
 where
  addMetric' = Just . maybe (S.singleton metric) (insertR metric)
  -- insert from the right, maintaining ordering
  insertR x Empty = S.singleton x
  insertR x (ys :|> y) =
    if fst x < fst y then insertR x ys |> y else ys |> y |> x

-- | Get the current throughput information for all interfaces as a map from
-- interface name to a tuple containing the number of packets and total number
-- of bytes to pass through that interface within the last second.
getThroughput :: UsageMonitor -> IO (Map String (Int, Int))
getThroughput monitor = unwrapKeys . calcThroughput <$> pruneOldMetrics monitor
 where
  calcThroughput = fmap (\ms -> (length ms, sum (snd <$> ms)))
  unwrapKeys     = M.mapKeys (\(IfaceName n) -> n)

module Hyperpipe.ThroughputTracker where

import Data.Map (Map(..))
import qualified Data.Map as M
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)

type IfaceName = String
type PacketSize = Int

type ThroughputTracker = Map IfaceName [(UTCTime, PacketSize)]


pruneOldMetrics :: ThroughputTracker -> IO ThroughputTracker
pruneOldMetrics tt = do
  time <- getCurrentTime
  return $ filter (\(pTime, pSize) -> diffUTCTime time pTime < 1.0) <$> tt

addMetric
  :: ThroughputTracker
  -> IfaceName
  -> (UTCTime, PacketSize)
  -> IO ThroughputTracker
addMetric tt iface (ts, size) = do
  let metrics = M.findWithDefault [] iface tt
  pruneOldMetrics (M.insert iface ((ts, size) : metrics) tt)

getThroughput :: ThroughputTracker -> IO (Map IfaceName (Int, Int))
getThroughput tt = do
  tt' <- pruneOldMetrics tt
  return $ (\ms -> (length ms, sum (snd <$> ms))) <$> tt'

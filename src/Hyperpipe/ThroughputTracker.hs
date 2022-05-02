module Hyperpipe.ThroughputTracker where

import Data.Map (Map(..))
import qualified Data.Map as M
import Data.Sequence (Seq(..), ViewR(..), (|>))
import qualified Data.Sequence as S
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)

type IfaceName = String
type PacketSize = Int

type ThroughputTracker = Map IfaceName (Seq (UTCTime, PacketSize))


pruneOldMetrics :: ThroughputTracker -> IO ThroughputTracker
pruneOldMetrics tt = do
  time <- getCurrentTime
  return $ S.dropWhileL (\(pTime, pSize) -> diffUTCTime time pTime > 1.0) <$> tt

addMetric
  :: ThroughputTracker
  -> IfaceName
  -> (UTCTime, PacketSize)
  -> IO ThroughputTracker
addMetric tt iface (ts, size) = do
  let metrics = M.findWithDefault Empty iface tt
  pruneOldMetrics (M.insert iface (insertSorted (ts, size) metrics) tt)
 where
  insertSorted (newTime, newSize) elems = case S.viewr elems of
    EmptyR                       -> elems |> (newTime, newSize)
    (xs :> (lastTime, lastSize)) -> if newTime < lastTime
      then insertSorted (newTime, newSize) xs |> (lastTime, lastSize)
      else elems |> (newTime, newSize)

getThroughput :: ThroughputTracker -> IO (Map IfaceName (Int, Int))
getThroughput tt = do
  tt' <- pruneOldMetrics tt
  return $ (\ms -> (length ms, sum (snd <$> ms))) <$> tt'

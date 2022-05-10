module Types where

import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Time.Clock (UTCTime)

-- * Convenient aliases
type Packets = Int
type Bytes = Int

-- * Internal state types
data Direction = Input | Output deriving (Eq, Show)
  
data IfaceInfo = IfaceInfo
  { vlanSetting  :: String
  , ifaceDir     :: Direction
  , usageHistory :: Seq (UTCTime, Packets, Bytes)
  }
  deriving Show

type State = Map String IfaceInfo

-- * Brick types
data Event = UpdateStuff (Map String (Packets, Bytes))


module Hyperpipe.EthFrame where

import Data.Binary (Binary(..))
import Data.Binary.Get
import Data.ByteString.Lazy (ByteString(..))
import qualified Data.ByteString.Lazy as B
import Data.Char (toUpper)
import Data.List (intercalate)
import Data.Word (Word16)
import Numeric (showHex)
import Data.Binary.Get (getWord16le)
import Data.Binary.Put (putWord16le)

-- | MAC Address
newtype MACAddr = MACAddr ByteString
  deriving (Eq)

-- | Custom show instance that writes MAC addresses like: "12:34:45:67:89:AB"
instance Show MACAddr where
  show (MACAddr bs) = intercalate ":" (map showByte $ B.unpack bs)
   where
    showByte w =
      let hex = map toUpper $ showHex w ""
      in if length hex == 1 then '0' : hex else hex

-- | 2-byte EtherType, specifies the protocol of the frame.
newtype EtherType = EtherType Word16
  deriving (Show, Eq)

-- | 2-byte VLAN tag
newtype VLANTag = VLANTag Word16
  deriving (Show, Eq)

-- | The ethernet frame structure,
data EthFrame = EthFrame
  { srcMac       :: MACAddr       -- ^ source MAC address
  , dstMac       :: MACAddr       -- ^ destination MAC address
  , ethType      :: EtherType     -- ^ EtherType of frame
  , frameVlan    :: Maybe VLANTag -- ^ VLAN tag of frame (if present)
  , framePayload :: ByteString    -- ^ payload (remainder of frame after EthType)
  }

instance Binary EthFrame where
  get = undefined
  put = undefined

instance Binary EtherType where
  get = do
    et <- getWord16le
    return $ EtherType et
  put (EtherType e) = do
    putWord16le e

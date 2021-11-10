{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hyperpipe.EthFrame where

import Data.Binary (Binary(..))
import Data.Binary.Get
import Data.Binary.Put (putByteString, putLazyByteString, putWord16be)
import Data.ByteString.Lazy (ByteString(..))
import qualified Data.ByteString.Lazy as BL
import Data.Char (toUpper)
import Data.List (intercalate)
import Data.Word (Word16)
import Numeric (showHex)
import Data.Maybe (isNothing)

-- | MAC Address
newtype MACAddr = MACAddr ByteString
  deriving (Eq)

-- | Custom show instance that writes MAC addresses like: "12:34:45:67:89:AB"
instance Show MACAddr where
  show (MACAddr bs) = intercalate ":" (map showByte $ BL.unpack bs)
   where
    showByte w =
      let hex = map toUpper $ showHex w ""
      in if length hex == 1 then '0' : hex else hex

-- | 2-byte EtherType, specifies the protocol of the frame.
newtype EtherType = EtherType Word16
  deriving (Show, Eq, Num)

-- | 2-byte VLAN tag
newtype VLANTag = VLANTag Word16
  deriving (Show, Eq)

instance Binary VLANTag where
    get = VLANTag <$> getWord16be
    put (VLANTag vt) = putWord16be vt

-- | The ethernet frame structure,
data EthFrame = EthFrame
  { dstMac       :: MACAddr       -- ^ destination MAC address
  , srcMac       :: MACAddr       -- ^ source MAC address
  , ethType      :: EtherType     -- ^ EtherType of frame
  , frameVlan    :: Maybe VLANTag -- ^ VLAN tag of frame (if present)
  , framePayload :: ByteString    -- ^ payload (remainder of frame after EthType)
  } deriving (Show, Eq)

instance Binary EthFrame where
  get = do
    dst     <- get
    src     <- get
    et      <- get
    vt <- if et == 0x8100 then Just <$> get else return Nothing
    et <- if isNothing vt then return et else get
    payload <- getRemainingLazyByteString
    return $ EthFrame {dstMac=dst, srcMac=src, ethType=et, frameVlan=vt, framePayload=payload}

  put (EthFrame mac1 mac2 et vt payload) = do
    put mac1
    put mac2
    put et
    case vt of 
      Nothing -> return ()
      Just vt' -> put vt'
    putLazyByteString payload

instance Binary EtherType where
  get = EtherType <$> getWord16be
  put (EtherType e) = putWord16be e

instance Binary MACAddr where
  get = do
    mac <- getLazyByteString 6
    return $ MACAddr mac

  put (MACAddr m) = putLazyByteString m

packet = BL.pack [72,93,54,244,186,194,0,216,97,54,118,90,8,0,69,0,0,76,85,18,64,0,64,17,228,7,192,168,1,245,109,200,209,33,162,120,195,87,0,56,1,209,175,205,0,6,0,2,70,93,192,209,115,85,252,209,209,39,198,76,233,196,110,170,250,247,240,131,85,199,92,132,136,166,165,177,48,86,74,239,32,53,217,39,37,46,55,98,0,128]
longPacket = BL.pack [0,216,97,54,118,90,72,93,54,244,186,194,8,0,69,40,3,144,145,112,64,0,56,17,172,61,109,200,209,33,192,168,1,245,195,87,162,120,3,124,148,255,144,101,151,111,89,24,72,105,0,2,70,30,190,222,0,3,76,60,177,87,205,85,108,225,133,239,19,153,228,164,145,108,143,59,50,2,217,180,91,73,249,16,141,0,16,63,219,183,136,204,150,53,15,235,122,210,50,225,194,5,6,4,182,76,122,123,83,204,229,2,43,75,131,40,160,67,80,45,202,212,216,189,38,231,81,221,186,240,184,195,157,77,241,192,48,108,133,102,125,177,133,139,248,56,222,142,243,199,236,94,195,150,172,161,200,233,230,19,23,155,19,187,124,86,42,101,89,236,217,244,95,189,170,240,203,251,53,105,101,5,209,122,244,200,192,31,37,2,50,248,122,197,40,84,208,96,107,14,61,185,128,238,32,188,124,109,115,218,18,30,31,129,143,33,115,240,251,156,137,249,54,149,207,236,50,191,113,168,6,109,28,4,2,252,182,122,28,127,129,146,140,60,243,190,67,84,153,14,77,86,154,117,180,78,163,246,240,52,116,33,237,1,5,90,89,99,168,143,255,251,226,108,190,162,73,27,137,48,217,102,128,228,15,182,222,192,253,100,80,246,106,116,160,237,196,176,232,152,8,43,43,42,121,150,172,102,119,49,181,205,68,104,23,134,16,78,80,180,88,114,50,94,139,252,82,51,229,210,117,7,46,10,53,85,168,191,253,225,107,210,215,135,52,233,91,182,142,209,247,255,188,28,204,175,206,175,88,115,17,12,62,246,40,51,170,153,129,37,108,209,229,101,234,90,181,137,184,7,63,108,241,58,145,146,90,37,214,171,41,117,72,96,4,64,219,40,29,133,187,224,11,55,12,78,102,146,112,17,156,13,172,236,153,168,225,95,136,183,183,137,27,141,242,179,233,45,1,18,76,112,79,90,88,197,81,136,247,153,239,28,125,233,10,126,225,102,70,175,179,2,202,142,37,108,39,91,153,170,54,121,136,68,249,233,132,176,88,55,21,224,209,193,143,56,246,139,187,105,196,133,157,169,22,21]
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module: EthFrame
--
-- An EthFrame consists of the Destination Mac, Source Mac, EtherType, optional
-- VLANTag, and remaining payload of a network packet.
--
-- The EthFrame's get and put functions will handle conversion between a lazy
-- ByteString and EthFrame.

module Hyperpipe.EthFrame where

import Data.Binary (Binary(..), decodeOrFail)
import Data.Binary.Get
import Data.Binary.Put (putByteString, putLazyByteString, putWord16be)
import Data.ByteString.Lazy (ByteString(..))
import qualified Data.ByteString.Lazy as BL
import Data.Char (toUpper)
import Data.List (intercalate)
import Data.Maybe (isNothing)
import Data.Word (Word16)
import Numeric (showHex)

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

-- | Custom Binary instance to put/get 6 bytes
instance Binary MACAddr where
  get = do
    m <- getLazyByteString 6
    return $ MACAddr m

  put (MACAddr m) = putLazyByteString m

-- | 2-byte EtherType, specifies the protocol of the frame.
newtype EtherType = EtherType Word16
  deriving (Show, Eq, Num)

-- | Simple Binary instance to get/put a Word16 in big endian
instance Binary EtherType where
  get = EtherType <$> getWord16be
  put (EtherType e) = putWord16be e

-- | 2-byte VLAN tag
newtype VLANTag = VLANTag Word16
  deriving (Show, Eq)

-- | A simple Binary instance to get/put a Word16 in big endian, like EtherType
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
  }
  deriving (Show, Eq)

-- | Binary instance to get/put an EthFrame
instance Binary EthFrame where
  get = do
    dst     <- get
    src     <- get
    et      <- get
    vt      <- if et == 0x8100 then Just <$> get else return Nothing
    et      <- if isNothing vt then return et else get
    payload <- getRemainingLazyByteString
    return $ EthFrame
      { dstMac       = dst
      , srcMac       = src
      , ethType      = et
      , frameVlan    = vt
      , framePayload = payload
      }

  put (EthFrame dst src et vt payload) = do
    put dst
    put src
    case vt of
      Nothing  -> return ()
      Just vt' -> putWord16be 0x8100 >> put vt'
    put et
    putLazyByteString payload

-- | Helper function for testing for Failure
parseFrame :: ByteString -> Either String EthFrame
parseFrame bs = case decodeOrFail bs of
  Left  (bs, bo, s ) -> Left s
  Right (bs, bo, et) -> Right et

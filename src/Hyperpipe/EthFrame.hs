{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module: EthFrame
--
-- An EthFrame consists of the Destination Mac, Source Mac, EtherType, optional
-- VLANTag, and remaining payload of a network packet.
--
-- The EthFrame's get and put functions will handle conversion between a lazy
-- ByteString and EthFrame.

module Hyperpipe.EthFrame where

import Data.ByteString (ByteString(..))
import qualified Data.ByteString as BS
import Data.Char (toUpper)
import Data.List (intercalate)
import Data.Maybe (isNothing)
import Data.Persist
  (Persist(..), getBE, getByteString, putBE, putByteString, remaining)
import Data.Word (Word16)
import Numeric (showHex)

-- | MAC Address
newtype MACAddr = MACAddr ByteString
  deriving (Eq)

-- | Custom show instance that writes MAC addresses like: "12:34:45:67:89:AB"
instance Show MACAddr where
  show (MACAddr bs) = intercalate ":" (map showByte $ BS.unpack bs)
   where
    showByte w =
      let hex = map toUpper $ showHex w ""
      in if length hex == 1 then '0' : hex else hex

-- | Custom Binary instance to put/get 6 bytes
instance Persist MACAddr where
  get = do
    m <- getByteString 6
    return $ MACAddr m

  put (MACAddr m) = putByteString m

-- | 2-byte EtherType, specifies the protocol of the frame.
newtype EtherType = EtherType Word16
  deriving (Show, Eq, Num)

-- | Simple Binary instance to get/put a Word16 in big endian
instance Persist EtherType where
  get = EtherType <$> getBE
  put (EtherType e) = putBE e

-- | 2-byte VLAN tag
newtype VLANTag = VLANTag Word16
  deriving (Show, Eq)

-- | A simple Binary instance to get/put a Word16 in big endian, like EtherType
instance Persist VLANTag where
  get = VLANTag <$> getBE
  put (VLANTag vt) = putBE vt

-- | The ethernet frame structure,
data EthFrame = EthFrame
  { dstMac       :: MACAddr       -- ^ destination MAC address
  , srcMac       :: MACAddr       -- ^ source MAC address
  , ethType      :: EtherType     -- ^ EtherType of frame
  , frameVlan    :: Maybe VLANTag -- ^ VLAN tag of frame (if present)
  , frameVlan2   :: Maybe VLANTag -- ^ 2nd VLAN tag of frame (if present and double tagged)
  , framePayload :: ByteString    -- ^ payload (remainder of frame after EthType)
  }
  deriving (Show, Eq)

-- | Binary instance to get/put an EthFrame
instance Persist EthFrame where
  get = do
    dst     <- get
    src     <- get
    et      <- get
    vt2     <- if et == 0x88A8 then Just <$> get else return Nothing
    et      <- if isNothing vt2 then return et else get
    vt      <- if et == 0x8100 then Just <$> get else return Nothing
    et      <- if isNothing vt then return et else get
    payload <- remaining >>= getByteString
    return $ EthFrame
      { dstMac       = dst
      , srcMac       = src
      , ethType      = et
      , frameVlan    = vt
      , frameVlan2   = vt2
      , framePayload = payload
      }

  put (EthFrame dst src et vt vt2 payload) = do
    put dst
    put src
    case vt2 of
      Nothing -> return()
      Just vt2' -> putBE (0x88A8 :: Word16) >> put vt2'
    case vt of
      Nothing  -> return ()
      Just vt' -> putBE (0x8100 :: Word16) >> put vt'
    put et
    putByteString payload

-- | Assigns a given VLAN tag to a frame (overwriting any existing tag)
setVlan :: VLANTag -> EthFrame -> EthFrame
setVlan vlan ef = ef { frameVlan = Just vlan }

-- | Removes any VLAN tag from the frame
stripVlan :: EthFrame -> EthFrame
stripVlan ef = ef { frameVlan = Nothing }

-- | Pretty-print frame header information
showFrameInfo :: EthFrame -> String
showFrameInfo ef =
  let
    src = show $ srcMac ef
    dst = show $ dstMac ef
    vlanInfo =
      maybe "" (\(VLANTag vt) -> "vlan: " ++ show vt ++ ", ") (frameVlan ef)
    EtherType et = ethType ef
    typeInfo     = "type: " ++ show et
  in src ++ " --> " ++ dst ++ " (" ++ vlanInfo ++ typeInfo ++ ")"

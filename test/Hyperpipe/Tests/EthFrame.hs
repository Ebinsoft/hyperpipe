module Hyperpipe.Tests.EthFrame where

import Data.Binary (decode, decodeOrFail, encode)
import Data.Binary.Get (ByteOffset)
import Data.ByteString.Lazy (ByteString(..))
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import Data.Word (Word8)
import Hyperpipe
import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary ByteString where
    arbitrary = BL.pack <$> arbitrary

instance Arbitrary MACAddr where
    arbitrary = MACAddr . BL.pack <$> sequence (take 6 $ repeat arbitrary)

instance Arbitrary EtherType where
    arbitrary = EtherType <$> arbitrary

instance Arbitrary VLANTag where
    arbitrary = VLANTag <$> arbitrary

instance Arbitrary EthFrame where
    arbitrary =
        EthFrame
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

tests :: TestTree
tests = testGroup
    "Hyperpipe.EthFrame"
    [ testProperty
        "Decoding ByteString to EthFrame and encoding back results in original ByteString"
        prop_identity
    , testProperty
        "decodeOrFailing EthFrame with too few bytes results in a Left"
        prop_short
    ]

prop_identity :: EthFrame -> Property
prop_identity ef = let bs = encode ef :: ByteString in decode bs === ef


prop_short :: ByteString -> Property
prop_short bs = BL.length bs < 14 ==> isLeft
    (decodeOrFail bs :: Either
          (ByteString, ByteOffset, String)
          (ByteString, ByteOffset, EthFrame)
    )

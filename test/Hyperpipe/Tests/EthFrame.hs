module Hyperpipe.Tests.EthFrame where

import Data.Binary (decode, encode)
import Data.ByteString.Lazy (ByteString(..))
import Data.ByteString.Lazy as BL
import Hyperpipe
import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Word (Word8)

instance Arbitrary ByteString where
    arbitrary = BL.pack <$> arbitrary

instance Arbitrary MACAddr where
    arbitrary = MACAddr . BL.pack <$> sequence (Prelude.take 6 $ Prelude.repeat arbitrary)

instance Arbitrary EtherType where
    arbitrary = EtherType <$> arbitrary

instance Arbitrary VLANTag where
    arbitrary = VLANTag <$> arbitrary

instance Arbitrary EthFrame where
    arbitrary = EthFrame <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

tests :: TestTree
tests = testGroup
    "Hyperpipe.EthFrame"
    [testProperty "Decode ByteString to EthFrame and encode back" prop_identity]

prop_identity :: EthFrame -> Property
prop_identity ef@(EthFrame dst src et vl p) =
    BL.length p >=1 ==>
    let bs = encode ef :: ByteString
    in decode bs === ef


prop_identity' :: ByteString -> Property
prop_identity' bs =
    BL.length bs >= 14 ==> 
    let ef = decode bs :: EthFrame in encode ef === bs

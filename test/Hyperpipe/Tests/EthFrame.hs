module Hyperpipe.Tests.EthFrame where

import Control.Monad (replicateM)
import Data.ByteString (ByteString(..))
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Serialize (decode, encode)
import Data.Word (Word8)
import Hyperpipe
import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary ByteString where
  arbitrary = BS.pack <$> arbitrary

instance Arbitrary MACAddr where
  arbitrary = MACAddr . BS.pack <$> replicateM 6 arbitrary

instance Arbitrary EtherType where
  arbitrary = EtherType <$> arbitrary

instance Arbitrary VLANTag where
  arbitrary = VLANTag <$> arbitrary <*> arbitrary

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
    prop_failOnTooFewBytes
  ]

prop_identity :: EthFrame -> Property
prop_identity ef = let bs = encode ef :: ByteString in decodeOrErr bs === ef

prop_failOnTooFewBytes :: ByteString -> Property
prop_failOnTooFewBytes bs =
  BS.length bs < 14 ==> isLeft (decode bs :: Either String EthFrame)

decodeOrErr :: ByteString -> EthFrame
decodeOrErr bs = case decode bs of
  Left  err -> error err
  Right ef  -> ef

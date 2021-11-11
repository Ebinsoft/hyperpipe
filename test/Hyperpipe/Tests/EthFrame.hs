module Hyperpipe.Tests.EthFrame where

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.ByteString.Lazy (ByteString(..))
import Data.ByteString.Lazy as BL
import Data.Binary (encode, decode)
import Hyperpipe

instance Arbitrary ByteString where
    arbitrary = return Hyperpipe.packet

tests :: TestTree
tests = testGroup
    "Hyperpipe.EthFrame"
    [ testProperty
        "Decode ByteString to EthFrame and encode back"
        prop_identity
    ]

prop_identity :: ByteString -> Bool
prop_identity bs = 
    let ef = decode bs :: EthFrame
    in encode ef == bs
module Main where

import Test.Tasty

import qualified Hyperpipe.Tests.ConfigParser
import qualified Hyperpipe.Tests.EthFrame
import qualified Hyperpipe.Tests.StateModel

main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite = testGroup
  "Hyperpipe"
  [ Hyperpipe.Tests.ConfigParser.tests
  , Hyperpipe.Tests.StateModel.tests
  , Hyperpipe.Tests.EthFrame.tests
  ]

module Main where

import Test.Tasty

import qualified Hyperpipe.Tests.ConfigParser

main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite = testGroup "hyperpipe" [Hyperpipe.Tests.ConfigParser.tests]

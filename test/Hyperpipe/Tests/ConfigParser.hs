{-# LANGUAGE QuasiQuotes #-}

module Hyperpipe.Tests.ConfigParser where

import Data.ByteString.Lazy (ByteString)
import Data.List (isInfixOf)
import Data.String.Interpolate (i)
import Test.Tasty
import Test.Tasty.HUnit

import Hyperpipe

tests :: TestTree
tests = testGroup
  "Hyperpipe.ConfigParser"
  [ testCase "Valid config parses successfully"
  $   parseCfgData validCfgYaml
  @=? Right validCfgModel
  , testCase "Invalid config returns error"
  $ (\(Left err) -> invalidCfgErr `isInfixOf` err) (parseCfgData invalidCfgYaml)
  @=? True
  ]

validCfgYaml :: ByteString
validCfgYaml = [i|
inputs:
  eno1:
    vlan: 0xAB
  eno2:
    vlan: 0x45

outputs:
  eno3:
  eno4:
    vlan: null
|]

validCfgModel = StateModel
  [ Endpoint
    { ifaceName  = IfaceName "eno1"
    , trafficDir = Input
    , frameOps   = [AddVLAN (VLANTag 0x8100 171)]
    }
  , Endpoint
    { ifaceName  = IfaceName "eno2"
    , trafficDir = Input
    , frameOps   = [AddVLAN (VLANTag 0x8100 69)]
    }
  , Endpoint
    { ifaceName  = IfaceName "eno3"
    , trafficDir = Output
    , frameOps   = []
    }
  , Endpoint
    { ifaceName  = IfaceName "eno4"
    , trafficDir = Output
    , frameOps   = [StripVLAN]
    }
  ]

invalidCfgYaml :: ByteString
invalidCfgYaml = [i|
inputs:
  eno1:
    vlan: 0xAB
  eno2:
    vlan: "bad vlan"

outputs:
  eno3:
  eno4:
    vlan: null
|]

invalidCfgErr :: String
invalidCfgErr = "expected VLAN tag instead of !!str"

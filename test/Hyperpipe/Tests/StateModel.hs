{-# LANGUAGE StandaloneDeriving #-}
module Hyperpipe.Tests.StateModel where

import Data.List (sort)
import Test.Tasty
import Test.Tasty.QuickCheck

import Hyperpipe

instance Arbitrary FlowDir where
  arbitrary = oneof $ return <$> [Input, Output]

instance Arbitrary FrameOp where
  arbitrary = oneof [SetVLAN . VLANTag <$> arbitrary, return StripVLAN]

instance Arbitrary IfaceName where
  arbitrary = IfaceName <$> arbitrary

instance Arbitrary Endpoint where
  arbitrary = Endpoint <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary StateModel where
  arbitrary = StateModel <$> arbitrary

-- deriving `Ord` implementations for all these types so we can compare sorted
-- lists of `Instruction`s
deriving instance Ord FlowDir
deriving instance Ord VLANTag
deriving instance Ord FrameOp
deriving instance Ord Endpoint
deriving instance Ord Instruction

tests :: TestTree
tests = testGroup
  "Hyperpipe.StateModel"
  [ testProperty
    "Transition from empty set enables all endpoints"
    prop_fromEmptySet
  , testProperty
    "Transition to empty set disables all endpoints"
    prop_toEmptySet
  , testProperty
    "Transition from a state to itself is empty list"
    prop_stateToItself
  , testProperty
    "Flipping transition direction generates inverse steps"
    prop_inverseSteps
  ]


prop_fromEmptySet :: StateModel -> Bool
prop_fromEmptySet sm@(StateModel es) =
  let empty = StateModel [] in stepsBetween empty sm == (EnableEndpoint <$> es)

prop_toEmptySet :: StateModel -> Bool
prop_toEmptySet sm@(StateModel es) =
  let empty = StateModel []
  in stepsBetween sm empty == (DisableEndpoint <$> es)

prop_stateToItself :: StateModel -> Bool
prop_stateToItself s = null $ stepsBetween s s

prop_inverseSteps :: StateModel -> StateModel -> Bool
prop_inverseSteps x y = sort (stepsBetween x y)
  == sort (flipInstruction <$> stepsBetween y x)
 where
  flipInstruction (EnableEndpoint  e) = DisableEndpoint e
  flipInstruction (DisableEndpoint e) = EnableEndpoint e

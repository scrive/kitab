{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Test.Driver.CiliumTests (test) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Test.Tasty

import CLI.Error
import Core.Model.ContextName
import Driver.Cilium (validateContextFilters)
import Test.Utils

test :: TestTree
test =
  testGroup
    "Driver.Cilium"
    [ testGroup
        "validateContextFilters"
        [ testThat "Known filters pass" testKnownFiltersPass
        , testThat "Empty filters pass" testEmptyFiltersPass
        , testThat "Single unknown filter is rejected" testSingleUnknownRejected
        , testThat "All unknown filters are reported together" testMultipleUnknownReported
        , testThat "Only unknown filters are reported" testMixedReportsOnlyUnknown
        ]
    ]

runValidate :: List ContextName -> List ContextName -> TestEff (Either (NonEmpty CLIError) Unit)
runValidate known filters =
  liftIO . runEff . runErrorNoCallStack @(NonEmpty CLIError) $
    validateContextFilters known filters

testKnownFiltersPass :: TestEff Unit
testKnownFiltersPass = do
  result <- runValidate ["a", "b"] ["a"]
  assertRight "Known filter should pass validation" result

testEmptyFiltersPass :: TestEff Unit
testEmptyFiltersPass = do
  result <- runValidate ["a"] []
  assertRight "Empty filter list should pass validation" result

testSingleUnknownRejected :: TestEff Unit
testSingleUnknownRejected = do
  result <- runValidate ["a"] ["zzz"]
  errors <- assertLeft "Unknown filter should be rejected" result
  assertEqual
    "Single unknown context filter"
    (NE.singleton (unknownContextFilter "zzz"))
    errors

testMultipleUnknownReported :: TestEff Unit
testMultipleUnknownReported = do
  result <- runValidate ["a"] ["x", "y"]
  errors <- assertLeft "Unknown filters should be rejected" result
  assertEqual
    "Both unknown filters reported, in order"
    (NE.fromList [unknownContextFilter "x", unknownContextFilter "y"])
    errors

testMixedReportsOnlyUnknown :: TestEff Unit
testMixedReportsOnlyUnknown = do
  result <- runValidate ["a", "b"] ["a", "zzz", "b"]
  errors <- assertLeft "Unknown filter among known ones should be rejected" result
  assertEqual
    "Only the unknown filter is reported"
    (NE.singleton (unknownContextFilter "zzz"))
    errors

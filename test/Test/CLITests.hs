module Test.CLITests (test) where

import Test.Tasty
import Text.Megaparsec (parse)

import CLI.Filter.Parser
import Core.Filtering.Types
import Test.Utils

test :: TestTree
test =
  testGroup
    "CLI"
    [ testGroup
        "Option parsing"
        [ testGroup
            "Filtering"
            [ testThat "Equality filter without surrounding spaces" testEqualityFilterWithoutSurroundingSpaces
            , testThat "Equality filter with surrounding spaces" testEqualityFilterWithSurroundingSpaces
            ]
        ]
    ]

testEqualityFilterWithoutSurroundingSpaces :: TestEff ()
testEqualityFilterWithoutSurroundingSpaces = do
  filterAction <- assertRight "Could not parse equality exprssion" $ parse cmpParser "EXPRESSION" "env==prod"
  assertEqual
    "Filter action is Equals"
    (Equals "env" "prod")
    filterAction

testEqualityFilterWithSurroundingSpaces :: TestEff ()
testEqualityFilterWithSurroundingSpaces = do
  filterAction <- assertRight "Could not parse equality exprssion" $ parse cmpParser "EXPRESSION" "env == prod"
  assertEqual
    "Filter action is Equals"
    (Equals "env" "prod")
    filterAction

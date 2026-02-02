module Main (main) where

import Test.Tasty

import Test.ModelTests qualified as ModelTests
import Test.ParserTests qualified as ParserTests
import Test.Render.C4Tests qualified as C4Tests
import Test.Render.CiliumTests qualified as CiliumTests

main :: IO ()
main =
  defaultMain $ testGroup "Kitab Tests" tests

tests :: [TestTree]
tests =
  [ ModelTests.test
  , ParserTests.test
  , C4Tests.test
  , CiliumTests.test
  ]

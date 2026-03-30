module Main (main) where

import Test.Tasty

import Test.InventoryTests qualified as InventoryTests
import Test.ModelTests qualified as ModelTests
import Test.ParserTests qualified as ParserTests
import Test.Render.C4Tests qualified as C4Tests
import Test.Render.CiliumTests qualified as CiliumTests
import Test.Render.GEXFTests qualified as GEXFTests

main :: IO ()
main =
  defaultMain $ testGroup "Kitab Tests" tests

tests :: [TestTree]
tests =
  [ ModelTests.test
  , ParserTests.test
  , C4Tests.test
  , CiliumTests.test
  , InventoryTests.test
  , GEXFTests.test
  ]

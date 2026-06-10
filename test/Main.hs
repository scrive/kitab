module Main (main) where

import Test.Tasty

import Test.InventoryTests qualified as InventoryTests
import Test.ModelTests qualified as ModelTests
import Test.ParserTests qualified as ParserTests
import Test.Render.CiliumTests qualified as CiliumTests
import Test.Render.GEXFTests qualified as GEXFTests
import Test.Render.PumlTests qualified as PumlTests

main :: IO Unit
main =
  defaultMain $ testGroup "Kitab Tests" tests

tests :: List TestTree
tests =
  [ ModelTests.test
  , ParserTests.test
  , PumlTests.test
  , CiliumTests.test
  , InventoryTests.test
  , GEXFTests.test
  ]

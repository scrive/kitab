module Main (main) where

import Test.Tasty

import Test.CLI.DumpTests qualified as DumpTests
import Test.Driver.CiliumTests qualified as Driver.CiliumTests
import Test.InventoryTests qualified as InventoryTests
import Test.ModelTests qualified as ModelTests
import Test.ParserTests qualified as ParserTests
import Test.Render.CiliumTests qualified as Render.CiliumTests
import Test.Render.GEXFTests qualified as Render.GEXFTests
import Test.Render.PumlTests qualified as Render.PumlTests

main :: IO Unit
main =
  defaultMain $
    testGroup
      "Kitab Tests"
      [ testGroup "Model Tests" modelTests
      , testGroup "Render Tests" renderTests
      , testGroup "Driver Tests" driverTests
      ]

modelTests :: List TestTree
modelTests =
  [ ModelTests.test
  , DumpTests.test
  , ParserTests.test
  , InventoryTests.test
  ]

renderTests :: List TestTree
renderTests =
  [ Render.PumlTests.test
  , Render.CiliumTests.test
  , Render.GEXFTests.test
  ]

driverTests :: List TestTree
driverTests =
  [ Driver.CiliumTests.test
  ]

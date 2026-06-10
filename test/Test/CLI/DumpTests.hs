module Test.CLI.DumpTests where

import Data.ByteString.Lazy (LazyByteString)
import Data.Text.Lazy.Encoding qualified as TL
import Test.Tasty
import Test.Tasty.Golden
import Text.Pretty.Simple (pShowNoColor)

import Test.Utils

test :: TestTree
test =
  testGroup
    "Dump"
    [ goldenVsStringDiff
        "Dumps the parsed Haskell AST"
        diffCmd
        "test/golden/dumped-service-definition.hs"
        testDumpServiceDefinition
    ]

testDumpServiceDefinition :: IO LazyByteString
testDumpServiceDefinition = runTestEff $ do
  declarations <- assertParseDocument "test/fixtures/service-definition.kdl"
  pure . TL.encodeUtf8 $ pShowNoColor declarations

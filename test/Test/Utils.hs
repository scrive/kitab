module Test.Utils
  ( TestEff
  , runTestEff
  , testThat
  , assertEqual
  , assertBool
  , assertFailure
  , assertJust
  , assertNothing
  , assertRight
  , assertLeft
  , diffCmd
  , assertParse
  , assertParseFile
  ) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Effectful
import Effectful.FileSystem
import GHC.Stack
import KDL
import Test.Tasty (TestTree)
import Test.Tasty.HUnit qualified as Test

type TestEff a =
  Eff
    '[ FileSystem
     , IOE
     ]
    a

runTestEff :: TestEff a -> IO a
runTestEff action =
  action
    & runFileSystem
    & runEff

testThat :: String -> TestEff () -> TestTree
testThat name assertion =
  Test.testCase name $ runTestEff assertion

assertEqual :: (Eq a, HasCallStack, Show a) => String -> a -> a -> TestEff ()
assertEqual message expected actual = liftIO $ Test.assertEqual message expected actual

assertBool :: HasCallStack => String -> Bool -> TestEff ()
assertBool message assertion = liftIO $ Test.assertBool message assertion

assertFailure :: HasCallStack => String -> TestEff a
assertFailure = liftIO . Test.assertFailure

assertJust :: HasCallStack => String -> Maybe a -> TestEff a
assertJust _ (Just a) = pure a
assertJust message Nothing = liftIO $ Test.assertFailure message

assertNothing :: HasCallStack => String -> Maybe a -> TestEff ()
assertNothing message (Just _) = liftIO $ Test.assertFailure message
assertNothing _ Nothing = pure ()

assertRight :: (HasCallStack, Show a) => String -> Either a b -> TestEff b
assertRight _ (Right b) = pure b
assertRight message (Left a) = liftIO . Test.assertFailure $ (message <> ". Found " <> show a)

assertParse :: HasCallStack => DocumentDecoder a -> Text -> TestEff a
assertParse decoder input =
  case KDL.decodeWith decoder input of
    Left decodeError -> assertFailure (T.unpack $ renderDecodeError decodeError)
    Right result -> pure result

assertParseFile :: HasCallStack => DocumentDecoder a -> FilePath -> TestEff a
assertParseFile decoder filePath =
  liftIO (KDL.decodeFileWith decoder filePath) >>= \case
    Left decodeError -> assertFailure (T.unpack $ renderDecodeError decodeError)
    Right result -> pure result

assertLeft :: HasCallStack => String -> Either a b -> TestEff a
assertLeft description (Right _b) = liftIO $ Test.assertFailure description
assertLeft _ (Left a) = pure a

diffCmd :: String -> String -> [String]
diffCmd ref new = ["diff", "-u", ref, new]

module Test.Utils
  ( TestEff
  , testThat
  , assertEqual
  , assertBool
  , assertFailure
  , assertJust
  , assertNothing
  , assertRight
  , assertLeft
  ) where

import Data.Function ((&))
import Effectful
import Effectful.FileSystem
import GHC.Stack
import Test.Tasty (TestTree)
import Test.Tasty.HUnit qualified as Test

type TestEff a =
  Eff
    '[ FileSystem
     , IOE
     ]
    a

testThat :: String -> TestEff () -> TestTree
testThat name assertion =
  Test.testCase name $
    assertion
      & runFileSystem
      & runEff

assertEqual :: (Eq a, HasCallStack, Show a) => String -> a -> a -> TestEff ()
assertEqual message expected actual = liftIO $ Test.assertEqual message expected actual

assertBool :: HasCallStack => String -> Bool -> TestEff ()
assertBool message assertion = liftIO $ Test.assertBool message assertion

assertFailure :: HasCallStack => String -> TestEff ()
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

assertLeft :: HasCallStack => String -> Either a b -> TestEff a
assertLeft description (Right _b) = liftIO $ Test.assertFailure description
assertLeft _ (Left a) = pure a

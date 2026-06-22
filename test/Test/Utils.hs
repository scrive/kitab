{-# LANGUAGE RecordWildCards #-}

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
  , assertParseDocument
  , assertParseError
  , assertDecodeError
  , runCapturingErrors
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack, runErrorWith)
import Effectful.FileSystem
import Effectful.FileSystem.IO.ByteString qualified as Filesystem
import GHC.Stack
import KDL
import Test.Tasty (TestTree)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.HUnit qualified as Test

import CLI.Error
import Core.Variable
import Parser (parseKitabDocument)
import Parser.V1.Types

type TestEff a =
  Eff
    [ FileSystem
    , Error (NonEmpty CLIError)
    , IOE
    ]
    a

runTestEff :: HasCallStack => TestEff a -> IO a
runTestEff action =
  action
    & runFileSystem
    & runErrorWith handleTestError
    & runEff
  where
    handleTestError :: IOE :> es => CallStack -> NonEmpty CLIError -> Eff es b
    handleTestError cs errs = do
      let prettyCS = unlines $ map formatFunCall (getCallStack cs)
      -- liftIO $ BS8.hPutStrLn stderr ("\n" <> BS8.pack )
      let message =
            concatMap (T.unpack . display) errs
              <> "\n"
              <> "\t"
              <> prettyCS
      liftIO $ HUnit.assertFailure ("Test failure.\n\t" <> message)
    formatFunCall :: Tuple2 String SrcLoc -> String
    formatFunCall (fun, SrcLoc {..}) =
      fun
        ++ " at "
        ++ srcLocFile
        ++ ":"
        ++ show srcLocStartLine
        ++ ":"
        ++ show srcLocStartCol

testThat :: HasCallStack => String -> TestEff Unit -> TestTree
testThat name assertion =
  Test.testCase name $ runTestEff assertion

assertEqual :: (Eq a, HasCallStack, Show a) => String -> a -> a -> TestEff Unit
assertEqual message expected actual = liftIO $ Test.assertEqual message expected actual

assertBool :: HasCallStack => String -> Bool -> TestEff Unit
assertBool message assertion = liftIO $ Test.assertBool message assertion

assertFailure :: HasCallStack => String -> TestEff a
assertFailure = liftIO . Test.assertFailure

assertJust :: HasCallStack => String -> Maybe a -> TestEff a
assertJust _ (Just a) = pure a
assertJust message Nothing = liftIO $ Test.assertFailure message

assertNothing :: HasCallStack => String -> Maybe a -> TestEff Unit
assertNothing message (Just _) = liftIO $ Test.assertFailure message
assertNothing _ Nothing = pure ()

assertRight :: (HasCallStack, Show a) => String -> Either a b -> TestEff b
assertRight _ (Right b) = pure b
assertRight message (Left a) = liftIO . Test.assertFailure $ message <> ". Found " <> show a

assertParseDocument :: HasCallStack => FilePath -> TestEff (List (Declaration Var))
assertParseDocument filePath = do
  content <- Filesystem.readFile filePath
  case parseKitabDocument filePath (T.decodeUtf8 content) of
    Left decodeError -> assertFailure (T.unpack $ renderDecodeError decodeError)
    Right result -> pure result

assertParseError :: HasCallStack => FilePath -> List Text -> TestEff Unit
assertParseError filePath expectedError = do
  content <- Filesystem.readFile filePath
  let prettyError = T.intercalate "\n" expectedError
  case parseKitabDocument filePath (T.decodeUtf8 content) of
    Left decodeError
      | KDL.renderDecodeError decodeError == prettyError -> pure ()
    Left actual -> assertFailure (T.unpack ("Expected " <> T.show (T.lines prettyError) <> " but got " <> T.show (T.lines $ KDL.renderDecodeError actual)))
    Right actual -> assertFailure (T.unpack ("Expected " <> prettyError <> " but got " <> T.pack (show actual)))

assertParse :: HasCallStack => DocumentDecoder a -> FilePath -> TestEff a
assertParse decoder filePath =
  liftIO (KDL.decodeFileWith decoder filePath) >>= \case
    Left decodeError -> assertFailure (T.unpack $ renderDecodeError decodeError)
    Right result -> pure result

-- | Assert that decoding fails, returning the rendered error for inspection.
-- The dual of 'assertParse', for negative parser tests.
assertDecodeError :: HasCallStack => DocumentDecoder a -> FilePath -> TestEff Text
assertDecodeError decoder filePath =
  liftIO (KDL.decodeFileWith decoder filePath) >>= \case
    Left decodeError -> pure (renderDecodeError decodeError)
    Right _ -> assertFailure ("Expected parse failure for " <> filePath <> " but parsing succeeded")

-- | Run an error-accumulating effectful action, capturing the thrown errors
-- instead of letting the 'TestEff' harness fail the test. For testing
-- validators and resolvers that throw 'NonEmpty CLIError' on the failure path.
runCapturingErrors
  :: Eff [Error (NonEmpty CLIError), IOE] a
  -> TestEff (Either (NonEmpty CLIError) a)
runCapturingErrors action = liftIO (runEff (runErrorNoCallStack action))

assertLeft :: HasCallStack => String -> Either a b -> TestEff a
assertLeft description (Right _b) = liftIO $ Test.assertFailure description
assertLeft _ (Left a) = pure a

diffCmd :: String -> String -> List String
diffCmd ref new = ["diff", "-u", ref, new]

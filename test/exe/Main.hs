-- Copyright (c) 2019-2020 The DAML and HLS Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Applicative.Combinators
import Control.Exception (catch)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.Foldable
import Data.List
import Data.Rope.UTF16 (Rope)
import qualified Data.Rope.UTF16 as Rope
import Development.IDE.Core.PositionMapping (fromCurrent, toCurrent)
import Development.IDE.GHC.Util
import qualified Data.Text as T
import Development.IDE.Spans.Common
-- import Development.IDE.Test
-- import Development.IDE.Test.Runfiles
import Development.IDE.Types.Location
import qualified Language.Haskell.LSP.Test as LSPTest
import Language.Haskell.LSP.Test hiding (openDoc')
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Capabilities
import Language.Haskell.LSP.VFS (applyChange)
import System.Environment.Blank (setEnv)
import System.FilePath
import System.IO.Extra
import System.Directory
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Maybe

import TestUtils

-- ---------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "HLS"
  [ testSession "open close" $ do
      doc <- openDoc' "Testing.hs" "haskell" ""
      void (message :: Session WorkDoneProgressCreateRequest)
      void (message :: Session WorkDoneProgressBeginNotification)
      closeDoc doc
      void (message :: Session WorkDoneProgressEndNotification)
  ]

----------------------------------------------------------------------
-- Utils


testSession :: String -> Session () -> TestTree
testSession name = testCase name . run

{-
testSessionWait :: String -> Session () -> TestTree
testSessionWait name = testSession name .
      -- Check that any diagnostics produced were already consumed by the test case.
      --
      -- If in future we add test cases where we don't care about checking the diagnostics,
      -- this could move elsewhere.
      --
      -- Experimentally, 0.5s seems to be long enough to wait for any final diagnostics to appear.
      ( >> expectNoMoreDiagnostics 0.5)

pickActionWithTitle :: T.Text -> [CAResult] -> CodeAction
pickActionWithTitle title actions = head
  [ action
  | CACodeAction action@CodeAction{ _title = actionTitle } <- actions
  , title == actionTitle ]
-}

mkRange :: Int -> Int -> Int -> Int -> Range
mkRange a b c d = Range (Position a b) (Position c d)

run :: Session a -> IO a
run s = withTempDir $ \dir -> do
  let ghcideExe = hieCommand

  -- Temporarily hack around https://github.com/mpickering/hie-bios/pull/56
  -- since the package import test creates "Data/List.hs", which otherwise has no physical home
  createDirectoryIfMissing True $ dir ++ "/Data"

  let cmd = unwords [ghcideExe, "--lsp", "--cwd", dir]
  -- HIE calls getXgdDirectory which assumes that HOME is set.
  -- Only sets HOME if it wasn't already set.
  setEnv "HOME" "/homeless-shelter" False
  let lspTestCaps = fullCaps { _window = Just $ WindowClientCapabilities $ Just True }
  runSessionWithConfig conf cmd lspTestCaps dir s
  where
    conf = defaultConfig
      -- If you uncomment this you can see all logging
      -- which can be quite useful for debugging.
      -- { logStdErr = True, logColor = False }
      -- If you really want to, you can also see all messages
      -- { logMessages = True, logColor = False }

openTestDataDoc :: FilePath -> Session TextDocumentIdentifier
openTestDataDoc path = do
  source <- liftIO $ readFileUtf8 $ "test/data" </> path
  openDoc' path "haskell" source

findCodeActions :: TextDocumentIdentifier -> Range -> [T.Text] -> Session [CodeAction]
findCodeActions doc range expectedTitles = do
  actions <- getCodeActions doc range
  let matches = sequence
        [ listToMaybe
          [ action
          | CACodeAction action@CodeAction { _title = actionTitle } <- actions
          , actionTitle == expectedTitle ]
        | expectedTitle <- expectedTitles]
  let msg = show
            [ actionTitle
            | CACodeAction CodeAction { _title = actionTitle } <- actions
            ]
            ++ "is not a superset of "
            ++ show expectedTitles
  liftIO $ case matches of
    Nothing -> assertFailure msg
    Just _ -> pure ()
  return (fromJust matches)

findCodeAction :: TextDocumentIdentifier -> Range -> T.Text -> Session CodeAction
findCodeAction doc range t = head <$> findCodeActions doc range [t]

-- | Wrapper around 'LSPTest.openDoc'' that sends file creation events
openDoc' :: FilePath -> String -> T.Text -> Session TextDocumentIdentifier
openDoc' fp name contents = do
  res@(TextDocumentIdentifier uri) <- LSPTest.openDoc' fp name contents
  sendNotification WorkspaceDidChangeWatchedFiles (DidChangeWatchedFilesParams $ List [FileEvent uri FcCreated])
  return res

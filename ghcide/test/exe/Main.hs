-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Control.Monad (void)
import qualified Data.Text as T
import Development.IDE.Test
import Development.IDE.Test.Runfiles
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import System.Environment.Blank (setEnv)
import System.IO.Extra
import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain $ testGroup "HIE"
  [ testSession "open close" $ do
      doc <- openDoc' "Testing.hs" "haskell" ""
      void (message :: Session ProgressStartNotification)
      closeDoc doc
      void (message :: Session ProgressDoneNotification)
  , testSession "fix syntax error" $ do
      let content = T.unlines [ "module Testing wher" ]
      doc <- openDoc' "Testing.hs" "haskell" content
      expectDiagnostics [("Testing.hs", [(DsError, (0, 15), "parse error")])]
      let change = TextDocumentContentChangeEvent
            { _range = Just (Range (Position 0 15) (Position 0 19))
            , _rangeLength = Nothing
            , _text = "where"
            }
      changeDoc doc [change]
      expectDiagnostics [("Testing.hs", [])]
  , testSession "introduce syntax error" $ do
      let content = T.unlines [ "module Testing where" ]
      doc <- openDoc' "Testing.hs" "haskell" content
      void (message :: Session ProgressStartNotification)
      let change = TextDocumentContentChangeEvent
            { _range = Just (Range (Position 0 15) (Position 0 18))
            , _rangeLength = Nothing
            , _text = "wher"
            }
      changeDoc doc [change]
      expectDiagnostics [("Testing.hs", [(DsError, (0, 15), "parse error")])]
  ]


----------------------------------------------------------------------
-- Utils


testSession :: String -> Session () -> TestTree
testSession name = testCase name . run


run :: Session a -> IO a
run s = withTempDir $ \dir -> do
  hieCoreExe <- locateHieCoreExecutable
  let cmd = unwords [hieCoreExe, "--lsp", "--cwd", dir]
  -- HIE calls getXgdDirectory which assumes that HOME is set.
  -- Only sets HOME if it wasn't already set.
  setEnv "HOME" "/homeless-shelter" False
  runSessionWithConfig conf cmd fullCaps dir s
  where
    conf = defaultConfig
      -- If you uncomment this you can see all messages
      -- which can be quite useful for debugging.
      -- { logMessages = True, logColor = False, logStdErr = True }

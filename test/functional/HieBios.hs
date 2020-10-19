{-# LANGUAGE OverloadedStrings #-}
module HieBios (tests) where

import Control.Applicative.Combinators
import qualified Data.Text as T
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Messages
import System.FilePath ((</>))
import Test.Hls.Util
import Test.Tasty
import Test.Tasty.ExpectedFailure (ignoreTestBecause)
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "hie-bios" [
    ignoreTestBecause "Broken" $ testCase "loads modules inside main-is" $ do
        writeFile (hieBiosErrorPath </> "hie.yaml") ""
        runSession hlsCommand fullCaps "test/testdata/hieBiosMainIs" $ do
            _ <- openDoc "Main.hs" "haskell"
            _ <- count 2 waitForDiagnostics
            return ()

    , ignoreTestBecause "Broken" $ testCase "reports errors in hie.yaml" $ do
        writeFile (hieBiosErrorPath </> "hie.yaml") ""
        runSession hlsCommand fullCaps hieBiosErrorPath $ do
            _ <- openDoc "Foo.hs" "haskell"
            _ <- skipManyTill loggingNotification (satisfy isMessage)
            return ()
    ]
    where
        hieBiosErrorPath = "test/testdata/hieBiosError"

        isMessage (NotShowMessage (NotificationMessage _ _ (ShowMessageParams MtError s))) =
            "Couldn't parse hie.yaml" `T.isInfixOf` s
        isMessage _ = False

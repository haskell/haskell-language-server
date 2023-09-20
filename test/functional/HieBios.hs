{-# LANGUAGE OverloadedStrings #-}
module HieBios (tests) where

import           Control.Lens               ((^.))
import           Control.Monad.IO.Class
import qualified Data.Text                  as T
import qualified Language.LSP.Protocol.Lens as L
import           Test.Hls
import           Test.Hls.Command

tests :: TestTree
tests = testGroup "hie-bios"
  [ testCase "loads main-is module" $ do
    runSession hlsCommand fullCaps "test/testdata/hieBiosMainIs" $ do
      _ <- openDoc "Main.hs" "haskell"
      (diag:_) <- waitForDiagnostics
      liftIO $ "Top-level binding with no type signature:" `T.isInfixOf` (diag ^. L.message)
        @? "Expected missing top-level binding diagnostic"

  , expectFailBecause "hie-bios 0.11 has poor error messages" $ testCase "reports errors in hie.yaml" $ do
    runSession hlsCommand fullCaps "test/testdata/hieBiosError" $ do
      _ <- openDoc "Foo.hs" "haskell"
      (diag:_) <- waitForDiagnostics
      liftIO $ "Expected a cradle: key containing the preferences" `T.isInfixOf` (diag ^. L.message)
        @? "Expected missing top-level binding diagnostic"
  ]

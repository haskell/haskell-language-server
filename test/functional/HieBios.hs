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
    runSession hlsLspCommand fullLatestClientCaps "test/testdata/hieBiosMainIs" $ do
      _ <- openDoc "Main.hs" "haskell"
      (diag:_) <- waitForDiagnostics
      liftIO $ "Top-level binding with no type signature:" `T.isInfixOf` (diag ^. L.message)
        @? "Expected missing top-level binding diagnostic"
  ]

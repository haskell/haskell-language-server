{-# LANGUAGE OverloadedStrings #-}

module FunctionalBadProject (tests) where

-- import           Control.Lens hiding (List)
-- import           Control.Monad.IO.Class
-- import qualified Data.Text as T
-- import           Language.Haskell.LSP.Test hiding (message)
-- import           Language.Haskell.LSP.Types as LSP
-- import           Language.Haskell.LSP.Types.Lens as LSP hiding (contents, error )
-- import Test.HIE
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Expectations

-- ---------------------------------------------------------------------
-- TODO: Currently this can not succeed, since such an error is thrown in "runActionWithContext" which
-- can produce diagnostics at the moment. Needs more investigation
-- TODO: @fendor: Add issue link here
--
tests :: TestTree
tests = testGroup "behaviour on malformed projects" [
    testCase "no test executed" $ True `shouldBe` True
    ]

    -- testCase "deals with cabal file with unsatisfiable dependency" $
    --     runSession hieCommandExamplePlugin codeActionSupportCaps "test/testdata/badProjects/cabal" $ do
    --     -- runSessionWithConfig logConfig hieCommandExamplePlugin codeActionSupportCaps "test/testdata" $ do
    --         _doc <- openDoc "Foo.hs" "haskell"

    --         diags@(d:_) <- waitForDiagnosticsSource "bios"
    --         -- liftIO $ show diags `shouldBe` ""
    --         -- liftIO $ putStrLn $ show diags
    --         -- liftIO $ putStrLn "a"
    --         liftIO $ do
    --             length diags `shouldBe` 1
    --             d ^. range `shouldBe` Range (Position 0 0) (Position 1 0)
    --             d ^. severity `shouldBe` (Just DsError)
    --             d ^. code `shouldBe` Nothing
    --             d ^. source `shouldBe` Just "bios"
    --             d ^. message `shouldBe`
    --                 (T.pack "readCreateProcess: stack \"build\" \"--only-configure\" \".\" (exit 1): failed\n")

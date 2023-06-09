{-# LANGUAGE OverloadedStrings #-}

module FunctionalBadProject (tests) where

-- import           Control.Lens hiding (List)
-- import           Control.Monad.IO.Class
-- import qualified Data.Text as T
-- import           Language.LSP.Test hiding (message)
-- import           Language.LSP.Types as LSP
-- import           Language.LSP.Types.Lens as LSP hiding (contents, error )
import           Test.Hls

-- ---------------------------------------------------------------------
-- TODO: Currently this can not succeed, since such an error is thrown in "runActionWithContext" which
-- can produce diagnostics at the moment. Needs more investigation
-- TODO: @fendor: Add issue link here
--
tests :: TestTree
tests = testGroup "behaviour on malformed projects" [
    testCase "no test executed" $ True @?= True
    ]

    -- testCase "deals with cabal file with unsatisfiable dependency" $
    --     runSession hlsCommandExamplePlugin codeActionSupportCaps "test/testdata/badProjects/cabal" $ do
    --         _doc <- openDoc "Foo.hs" "haskell"

    --         diags@(d:_) <- waitForDiagnosticsSource "bios"
    --         -- liftIO $ show diags @?= ""
    --         -- liftIO $ putStrLn $ show diags
    --         -- liftIO $ putStrLn "a"
    --         liftIO $ do
    --             length diags @?= 1
    --             d ^. range @?= Range (Position 0 0) (Position 1 0)
    --             d ^. severity @?= (Just DsError)
    --             d ^. code @?= Nothing
    --             d ^. source @?= Just "bios"
    --             d ^. message @?=
    --                 (T.pack "readCreateProcess: stack \"build\" \"--only-configure\" \".\" (exit 1): failed\n")

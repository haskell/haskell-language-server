module IfaceTests (tests) where

import           Config
import           Control.Monad.IO.Class        (liftIO)
import qualified Data.Text                     as T
import           Development.IDE.GHC.Util
import           Development.IDE.Test          (configureCheckProject,
                                                expectDiagnostics,
                                                expectNoMoreDiagnostics,
                                                getInterfaceFilesDir)
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types   hiding
                                               (SemanticTokenAbsolute (..),
                                                SemanticTokenRelative (..),
                                                SemanticTokensEdit (..),
                                                mkRange)
import           Language.LSP.Test
import           System.Directory
import           System.FilePath
import           Test.Hls.FileSystem
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Interface loading tests"
    [ -- https://github.com/haskell/ghcide/pull/645/
      ifaceErrorTest
    , ifaceErrorTest2
    , ifaceErrorTest3
    , ifaceTHTest
    , ifaceTransitivePropagationTest
    ]


-- | test that TH reevaluates across interfaces
ifaceTHTest :: TestTree
ifaceTHTest = testWithExtraFiles "iface-th-test" "TH" $ \dir -> do
    let aPath = dir </> "THA.hs"
        bPath = dir </> "THB.hs"
        cPath = dir </> "THC.hs"

    aSource <- liftIO $ readFileUtf8 aPath -- [TH] a :: ()
    _bSource <- liftIO $ readFileUtf8 bPath -- a :: ()
    cSource <- liftIO $ readFileUtf8 cPath -- c = a :: ()

    cdoc <- createDoc cPath "haskell" cSource

    -- Change [TH]a from () to Bool
    liftIO $ atomicFileWriteStringUTF8 aPath (unlines $ init (lines $ T.unpack aSource) ++ ["th_a = [d| a = False|]"])

    -- Check that the change propagates to C
    changeDoc cdoc [TextDocumentContentChangeEvent . InR $ TextDocumentContentChangeWholeDocument cSource]
    expectDiagnostics
      [("THC.hs", [(DiagnosticSeverity_Error, (4, 4), "Couldn't match expected type '()' with actual type 'Bool'", Just "GHC-83865")])
      ,("THB.hs", [(DiagnosticSeverity_Warning, (4,1), "Top-level binding", Just "GHC-38417")])]
    closeDoc cdoc

ifaceErrorTest :: TestTree
ifaceErrorTest = testWithExtraFiles "iface-error-test-1" "recomp" $ \dir -> do
    configureCheckProject True
    let bPath = dir </> "B.hs"
        pPath = dir </> "P.hs"

    bSource <- liftIO $ readFileUtf8 bPath -- y :: Int
    pSource <- liftIO $ readFileUtf8 pPath -- bar = x :: Int

    bdoc <- createDoc bPath "haskell" bSource
    expectDiagnostics
      [("P.hs", [(DiagnosticSeverity_Warning,(4,0), "Top-level binding", Just "GHC-38417")])] -- So what we know P has been loaded

    -- Change y from Int to B
    changeDoc bdoc [ TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $
                        T.unlines [ "module B where", "y :: Bool", "y = undefined"]
                   ]
    -- save so that we can that the error propagates to A
    sendNotification SMethod_TextDocumentDidSave (DidSaveTextDocumentParams bdoc Nothing)


    -- Check that the error propagates to A
    expectDiagnostics
      [("A.hs", [(DiagnosticSeverity_Error, (5, 4), "Couldn't match expected type 'Int' with actual type 'Bool'", Just "GHC-83865")])]

    -- Check that we wrote the interfaces for B when we saved
    hidir <- getInterfaceFilesDir bdoc
    hi_exists <- liftIO $ doesFileExist $ hidir </> "B.hi"
    liftIO $ assertBool ("Couldn't find B.hi in " ++ hidir) hi_exists

    pdoc <- openDoc pPath "haskell"
    expectDiagnostics
      [("P.hs", [(DiagnosticSeverity_Warning,(4,0), "Top-level binding", Just "GHC-38417")])
      ]
    changeDoc pdoc [TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $ pSource <> "\nfoo = y :: Bool" ]
    -- Now in P we have
    -- bar = x :: Int
    -- foo = y :: Bool
    -- HOWEVER, in A...
    -- x = y  :: Int
    -- This is clearly inconsistent, and the expected outcome a bit surprising:
    --   - The diagnostic for A has already been received. Ghcide does not repeat diagnostics
    --   - P is being typechecked with the last successful artifacts for A.
    expectDiagnostics
      [("P.hs", [(DiagnosticSeverity_Warning,(4,0), "Top-level binding", Just "GHC-38417")])
      ,("P.hs", [(DiagnosticSeverity_Warning,(6,0), "Top-level binding", Just "GHC-38417")])
      ]
    expectNoMoreDiagnostics 2

ifaceErrorTest2 :: TestTree
ifaceErrorTest2 = testWithExtraFiles "iface-error-test-2" "recomp" $ \dir -> do
    let bPath = dir </> "B.hs"
        pPath = dir </> "P.hs"

    bSource <- liftIO $ readFileUtf8 bPath -- y :: Int
    pSource <- liftIO $ readFileUtf8 pPath -- bar = x :: Int

    bdoc <- createDoc bPath "haskell" bSource
    pdoc <- createDoc pPath "haskell" pSource
    expectDiagnostics
      [("P.hs", [(DiagnosticSeverity_Warning,(4,0), "Top-level binding", Just "GHC-38417")])] -- So that we know P has been loaded

    -- Change y from Int to B
    changeDoc bdoc [TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $
        T.unlines ["module B where", "y :: Bool", "y = undefined"]]

    -- Add a new definition to P
    changeDoc pdoc [TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $ pSource <> "\nfoo = y :: Bool" ]
    -- Now in P we have
    -- bar = x :: Int
    -- foo = y :: Bool
    -- HOWEVER, in A...
    -- x = y  :: Int
    expectDiagnostics
    -- As in the other test, P is being typechecked with the last successful artifacts for A
    -- (ot thanks to -fdeferred-type-errors)
      [("A.hs", [(DiagnosticSeverity_Error, (5, 4), "Couldn't match expected type 'Int' with actual type 'Bool'", Just "GHC-83865")])
      ,("P.hs", [(DiagnosticSeverity_Warning, (4, 0), "Top-level binding", Just "GHC-38417")])
      ,("P.hs", [(DiagnosticSeverity_Warning, (6, 0), "Top-level binding", Just "GHC-38417")])
      ]

    expectNoMoreDiagnostics 2

-- | Saving a file should propagate type errors to its transitive reverse
-- dependencies, not just immediate importers. The fixture is a 3-module chain
-- M0 <- M1 <- M2 where M2 does not import M0 directly. Flipping M0's exported
-- type from Int to Bool must surface a diagnostic in M2.
ifaceTransitivePropagationTest :: TestTree
ifaceTransitivePropagationTest = testWithExtraFiles "iface-transitive-propagation" "transitive-recomp" $ \dir -> do
    configureCheckProject False
    let m0Path = dir </> "M0.hs"
        m2Path = dir </> "M2.hs"
    m0Source <- liftIO $ readFileUtf8 m0Path
    m2Source <- liftIO $ readFileUtf8 m2Path
    -- Open M2 first to bring the whole chain (M2 -> M1 -> M0) into the module
    -- graph. After this we close M2 so it leaves the FOI set.
    m2doc <- createDoc m2Path "haskell" m2Source
    expectDiagnostics
      [("M1.hs", [(DiagnosticSeverity_Warning, (4, 0), "Top-level binding", Just "GHC-38417")])]
    closeDoc m2doc
    m0doc <- createDoc m0Path "haskell" m0Source
    -- Flip M0's exported value from Int to Bool.
    changeDoc m0doc
      [TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $
          T.unlines ["module M0 (m0val) where", "m0val :: Bool", "m0val = True"]]
    sendNotification SMethod_TextDocumentDidSave (DidSaveTextDocumentParams m0doc Nothing)
    -- m1val is type-inferred and becomes Bool, so M2 (which has
    -- m2val :: Int = m1val) now fails. The M2 diagnostic only reaches the
    -- client if typecheckParents walks the transitive reverse-dep set.
    expectDiagnostics
      [ ("M1.hs", [(DiagnosticSeverity_Warning, (4, 0), "Top-level binding", Just "GHC-38417")])
      , ("M2.hs", [(DiagnosticSeverity_Error, (5, 8), "Couldn't match expected type 'Int' with actual type 'Bool'", Just "GHC-83865")])
      ]

ifaceErrorTest3 :: TestTree
ifaceErrorTest3 = testWithExtraFiles "iface-error-test-3" "recomp" $ \dir -> do
    let bPath = dir </> "B.hs"
        pPath = dir </> "P.hs"

    bSource <- liftIO $ readFileUtf8 bPath -- y :: Int
    pSource <- liftIO $ readFileUtf8 pPath -- bar = x :: Int

    bdoc <- createDoc bPath "haskell" bSource

    -- Change y from Int to B
    changeDoc bdoc [TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $ T.unlines ["module B where", "y :: Bool", "y = undefined"]]

    -- P should not typecheck, as there are no last valid artifacts for A
    _pdoc <- createDoc pPath "haskell" pSource

    -- In this example the interface file for A should not exist (modulo the cache folder)
    -- Despite that P still type checks, as we can generate an interface file for A thanks to -fdeferred-type-errors
    expectDiagnostics
      [("A.hs", [(DiagnosticSeverity_Error, (5, 4), "Couldn't match expected type 'Int' with actual type 'Bool'", Just "GHC-83865")])
      ,("P.hs", [(DiagnosticSeverity_Warning,(4,0), "Top-level binding", Just "GHC-38417")])
      ]
    expectNoMoreDiagnostics 2

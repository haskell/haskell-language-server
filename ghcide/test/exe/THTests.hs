
module THTests (tests) where

import           Config
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.Text                   as T
import           Development.IDE.GHC.Util
import           Development.IDE.Test        (expectCurrentDiagnostics,
                                              expectDiagnostics,
                                              expectNoMoreDiagnostics)
import           Language.LSP.Protocol.Types hiding (SemanticTokenAbsolute (..),
                                              SemanticTokenRelative (..),
                                              SemanticTokensEdit (..), mkRange)
import           Language.LSP.Test
import           System.FilePath
import           Test.Hls                    (waitForAllProgressDone,
                                              waitForProgressBegin)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "TemplateHaskell"
    [ -- Test for https://github.com/haskell/ghcide/pull/212
      testWithDummyPluginEmpty "load" $ do
        let sourceA =
              T.unlines
                [ "{-# LANGUAGE PackageImports #-}",
                  "{-# LANGUAGE TemplateHaskell #-}",
                  "module A where",
                  "import \"template-haskell\" Language.Haskell.TH",
                  "a :: Integer",
                  "a = $(litE $ IntegerL 3)"
                ]
            sourceB =
              T.unlines
                [ "{-# LANGUAGE PackageImports #-}",
                  "{-# LANGUAGE TemplateHaskell #-}",
                  "module B where",
                  "import A",
                  "import \"template-haskell\" Language.Haskell.TH",
                  "b :: Integer",
                  "b = $(litE $ IntegerL $ a) + n"
                ]
        _ <- createDoc "A.hs" "haskell" sourceA
        _ <- createDoc "B.hs" "haskell" sourceB
        expectDiagnostics [ ( "B.hs", [(DiagnosticSeverity_Error, (6, 29), "Variable not in scope: n")] ) ]
    , testWithDummyPluginEmpty "newtype-closure" $ do
        let sourceA =
              T.unlines
                [ "{-# LANGUAGE DeriveDataTypeable #-}"
                  ,"{-# LANGUAGE TemplateHaskell #-}"
                  ,"module A (a) where"
                  ,"import Data.Data"
                  ,"import Language.Haskell.TH"
                  ,"newtype A = A () deriving (Data)"
                  ,"a :: ExpQ"
                  ,"a = [| 0 |]"]
        let sourceB =
              T.unlines
                [ "{-# LANGUAGE TemplateHaskell #-}"
                ,"module B where"
                ,"import A"
                ,"b :: Int"
                ,"b = $( a )" ]
        _ <- createDoc "A.hs" "haskell" sourceA
        _ <- createDoc "B.hs" "haskell" sourceB
        return ()
    , thReloadingTest False
    , thLoadingTest
    , thCoreTest
    , thReloadingTest True
    -- Regression test for https://github.com/haskell/haskell-language-server/issues/891
    , thLinkingTest False
    , thLinkingTest True
    , testWithDummyPluginEmpty "findsTHIdentifiers" $ do
        let sourceA =
              T.unlines
                [ "{-# LANGUAGE TemplateHaskell #-}"
                , "module A (a) where"
                , "import Language.Haskell.TH (ExpQ)"
                , "a :: ExpQ" -- TH 2.17 requires an explicit type signature since splices are polymorphic
                , "a = [| glorifiedID |]"
                , "glorifiedID :: a -> a"
                , "glorifiedID = id" ]
        let sourceB =
              T.unlines
                [ "{-# OPTIONS_GHC -Wall #-}"
                , "{-# LANGUAGE TemplateHaskell #-}"
                , "module B where"
                , "import A"
                , "main = $a (putStrLn \"success!\")"]
        _ <- createDoc "A.hs" "haskell" sourceA
        _ <- createDoc "B.hs" "haskell" sourceB
        expectDiagnostics [ ( "B.hs", [(DiagnosticSeverity_Warning, (4, 0), "Top-level binding with no type signature: main :: IO ()")] ) ]
    , testCase "findsTHnewNameConstructor" $ runWithExtraFiles "THNewName" $ \dir -> do

    -- This test defines a TH value with the meaning "data A = A" in A.hs
    -- Loads and export the template in B.hs
    -- And checks wether the constructor A can be loaded in C.hs
    -- This test does not fail when either A and B get manually loaded before C.hs
    -- or when we remove the seemingly unnecessary TH pragma from C.hs

    let cPath = dir </> "C.hs"
    _ <- openDoc cPath "haskell"
    expectDiagnostics [ ( cPath, [(DiagnosticSeverity_Warning, (3, 0), "Top-level binding with no type signature: a :: A")] ) ]
    ]


-- | Test that all modules have linkables
thLoadingTest :: TestTree
thLoadingTest = testCase "Loading linkables" $ runWithExtraFiles "THLoading" $ \dir -> do
    let thb = dir </> "THB.hs"
    _ <- openDoc thb "haskell"
    expectNoMoreDiagnostics 1

thCoreTest :: TestTree
thCoreTest = testCase "Verifying TH core files" $ runWithExtraFiles "THCoreFile" $ \dir -> do
    let thc = dir </> "THC.hs"
    _ <- openDoc thc "haskell"
    expectNoMoreDiagnostics 1

-- | test that TH is reevaluated on typecheck
thReloadingTest :: Bool -> TestTree
thReloadingTest unboxed = testCase name $ runWithExtraFiles dir $ \dir -> do

    let aPath = dir </> "THA.hs"
        bPath = dir </> "THB.hs"
        cPath = dir </> "THC.hs"

    aSource <- liftIO $ readFileUtf8 aPath --  th = [d|a = ()|]
    bSource <- liftIO $ readFileUtf8 bPath --  $th
    cSource <- liftIO $ readFileUtf8 cPath --  c = a :: ()

    adoc <- createDoc aPath "haskell" aSource
    bdoc <- createDoc bPath "haskell" bSource
    cdoc <- createDoc cPath "haskell" cSource

    expectDiagnostics [("THB.hs", [(DiagnosticSeverity_Warning, (4,1), "Top-level binding")])]

    -- Change th from () to Bool
    let aSource' = T.unlines $ init (T.lines aSource) ++ ["th_a = [d| a = False|]"]
    changeDoc adoc [TextDocumentContentChangeEvent . InR $ TextDocumentContentChangeWholeDocument aSource']
    -- generate an artificial warning to avoid timing out if the TH change does not propagate
    changeDoc cdoc [TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $ cSource <> "\nfoo=()"]

    -- Check that the change propagates to C
    expectDiagnostics
        [("THC.hs", [(DiagnosticSeverity_Error, (4, 4), "Couldn't match expected type '()' with actual type 'Bool'")])
        ,("THC.hs", [(DiagnosticSeverity_Warning, (6,0), "Top-level binding")])
        ,("THB.hs", [(DiagnosticSeverity_Warning, (4,1), "Top-level bindin")])
        ]

    closeDoc adoc
    closeDoc bdoc
    closeDoc cdoc
  where
    name = "reloading-th-test" <> if unboxed then "-unboxed" else ""
    dir | unboxed = "THUnboxed"
        | otherwise = "TH"

thLinkingTest :: Bool -> TestTree
thLinkingTest unboxed = testCase name $ runWithExtraFiles dir $ \dir -> do

    let aPath = dir </> "THA.hs"
        bPath = dir </> "THB.hs"

    aSource <- liftIO $ readFileUtf8 aPath --  th_a = [d|a :: ()|]
    bSource <- liftIO $ readFileUtf8 bPath --  $th_a

    adoc <- createDoc aPath "haskell" aSource
    bdoc <- createDoc bPath "haskell" bSource

    expectDiagnostics [("THB.hs", [(DiagnosticSeverity_Warning, (4,1), "Top-level binding")])]

    let aSource' = T.unlines $ init (init (T.lines aSource)) ++ ["th :: DecsQ", "th = [d| a = False|]"]
    changeDoc adoc [TextDocumentContentChangeEvent . InR $ TextDocumentContentChangeWholeDocument aSource']

    -- modify b too
    let bSource' = T.unlines $ init (T.lines bSource) ++ ["$th"]
    changeDoc bdoc [TextDocumentContentChangeEvent . InR $ TextDocumentContentChangeWholeDocument bSource']
    waitForProgressBegin
    waitForAllProgressDone

    expectCurrentDiagnostics bdoc [(DiagnosticSeverity_Warning, (4,1), "Top-level binding")]

    closeDoc adoc
    closeDoc bdoc
  where
    name = "th-linking-test" <> if unboxed then "-unboxed" else ""
    dir | unboxed = "THUnboxed"
        | otherwise = "TH"

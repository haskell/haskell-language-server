{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval (
    tests,
) where

import Control.Applicative.Combinators (
    skipManyTill,
 )
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.LSP.Test (
    Session,
    anyMessage,
    documentContents,
    executeCommand,
    fullCaps,
    getCodeLenses,
    message,
    openDoc,
    runSession,
 )
import Language.Haskell.LSP.Types (
    ApplyWorkspaceEditRequest,
    CodeLens (CodeLens, _command, _range),
    Command (Command, _title),
    Position (..),
    Range (..),
    TextDocumentIdentifier,
 )
import System.Directory (doesFileExist)
import System.FilePath (
    (<.>),
    (</>),
 )
import Test.Hls.Util (hlsCommand)
import Test.Tasty (
    TestTree,
    testGroup,
 )
import Test.Tasty.ExpectedFailure (
    expectFailBecause,
    ignoreTestBecause,
 )
import Test.Tasty.HUnit (
    testCase,
    (@?=),
 )

tests :: TestTree
tests =
    testGroup
        "eval"
        [ testCase "Produces Evaluate code lenses" $
            runSession hlsCommand fullCaps evalPath $ do
                doc <- openDoc "T1.hs" "haskell"
                lenses <- getEvalCodeLenses doc
                liftIO $ map (fmap _title . _command) lenses @?= [Just "Evaluate..."]
        , testCase "Produces Refresh code lenses" $
            runSession hlsCommand fullCaps evalPath $ do
                doc <- openDoc "T2.hs" "haskell"
                lenses <- getEvalCodeLenses doc
                liftIO $ map (fmap _title . _command) lenses @?= [Just "Refresh..."]
        , testCase "Code lenses have ranges" $
            runSession hlsCommand fullCaps evalPath $ do
                doc <- openDoc "T1.hs" "haskell"
                lenses <- getEvalCodeLenses doc
                liftIO $ map _range lenses @?= [Range (Position 4 0) (Position 5 0)]
        , testCase "Multi-line expressions have a multi-line range" $ do
            runSession hlsCommand fullCaps evalPath $ do
                doc <- openDoc "T3.hs" "haskell"
                lenses <- getEvalCodeLenses doc
                liftIO $ map _range lenses @?= [Range (Position 3 0) (Position 5 0)]
        , testCase "Executed expressions range covers only the expression" $ do
            runSession hlsCommand fullCaps evalPath $ do
                doc <- openDoc "T2.hs" "haskell"
                lenses <- getEvalCodeLenses doc
                liftIO $ map _range lenses @?= [Range (Position 4 0) (Position 5 0)]
        , testCase "Evaluation of expressions" $ goldenTest "T1.hs"
        , testCase "Reevaluation of expressions" $ goldenTest "T2.hs"
        , testCase "Evaluation of expressions w/ imports" $ goldenTest "T3.hs"
        , testCase "Evaluation of expressions w/ lets" $ goldenTest "T4.hs"
        , testCase "Refresh an evaluation" $ goldenTest "T5.hs"
        , testCase "Refresh an evaluation w/ lets" $ goldenTest "T6.hs"
        , testCase "Refresh a multiline evaluation" $ goldenTest "T7.hs"
        , testCase "Semantic and Lexical errors are reported" $ goldenTest "T8.hs"
        , testCase "Applies file LANGUAGE extensions" $ goldenTest "T9.hs"
        , testCase "Evaluate a type with :kind!" $ goldenTest "T10.hs"
        , testCase "Reports an error for an incorrect type with :kind!" $
            goldenTest "T11.hs"
        , testCase "Shows a kind with :kind" $ goldenTest "T12.hs"
        , testCase "Reports an error for an incorrect type with :kind" $
            goldenTest "T13.hs"
        , testCase "Returns a fully-instantiated type for :type" $
            goldenTest "T14.hs"
        , testCase "Returns an uninstantiated type for :type +v, admitting multiple whitespaces around arguments" $
            goldenTest "T15.hs"
        , testCase "Returns defaulted type for :type +d, admitting multiple whitespaces around arguments" $
            goldenTest "T16.hs"
        , testCase ":type reports an error when given with unknown +x option" $
            goldenTest "T17.hs"
        , testCase "Reports an error when given with unknown command" $
            goldenTest "T18.hs"
        , testCase "Returns defaulted type for :type +d reflecting the default declaration specified in the >>> prompt" $
            goldenTest "T19.hs"
        , expectFailBecause "known issue - see a note in P.R. #361" $
            testCase ":type +d reflects the `default' declaration of the module" $
                goldenTest "T20.hs"
        , testCase ":type handles a multilined result properly" $
            goldenTest "T21.hs"
        , testCase ":t behaves exactly the same as :type" $
            goldenTest "T22.hs"
        , testCase ":type does \"dovetails\" for short identifiers" $
            goldenTest "T23.hs"
        , testCase ":kind! treats a multilined result properly" $
            goldenTest "T24.hs"
        , testCase ":kind treats a multilined result properly" $
            goldenTest "T25.hs"
        , testCase "local imports" $
            goldenTest "T26.hs"
        , testCase "Preserves one empty comment line after prompt" $
            goldenTest "T27.hs"
        , testCase
            "Multi line comments"
            $ goldenTest "TMulti.hs"
        , testCase
            "Evaluate expressions in Plain comments in both single line and multi line format"
            $ goldenTest "TPlainComment.hs"
        , testCase
            "Evaluate expressions in Haddock comments in both single line and multi line format"
            $ goldenTest "THaddock.hs"
        , testCase "Compare results (for Haddock tests only)" $
            goldenTest "TCompare.hs"
        , testCase "Local Modules imports are accessible in a test" $
            goldenTest "TLocalImport.hs"
        , -- , testCase "Local Modules can be imported in a test" $ goldenTest "TLocalImportInTest.hs"
          ignoreTestBecause "Unexplained but minor issue" $
            testCase "Setting language option TupleSections" $
                goldenTest "TLanguageOptionsTupleSections.hs"
        , testCase "IO expressions are supported, stdout/stderr output is ignored" $
            goldenTest "TIO.hs"
        , testCase "Property checking" $ goldenTest "TProperty.hs"
        , testCase
            "Prelude has no special treatment, it is imported as stated in the module"
            $ goldenTest "TPrelude.hs"
#if __GLASGOW_HASKELL__ >= 808
            , testCase "CPP support" $ goldenTest "TCPP.hs"
            , testCase "Literate Haskell Bird Style" $ goldenTest "TLHS.lhs"
#endif
            -- , testCase "Literate Haskell LaTeX Style" $ goldenTest "TLHSLateX.lhs"
        ]

goldenTest :: FilePath -> IO ()
goldenTest = goldenTestBy isEvalTest

{- |Execute all CodeLens accepted by 'filter'
 Compare results with the contents of corresponding '.expected' file (and creates it, if missing)
-}
goldenTestBy :: (CodeLens -> Bool) -> FilePath -> IO ()
goldenTestBy f input = runSession hlsCommand fullCaps evalPath $ do
    doc <- openDoc input "haskell"

    -- Execute lenses backwards, to avoid affecting their position in the source file
    codeLenses <- reverse <$> getCodeLensesBy f doc
    -- liftIO $ print codeLenses

    -- Execute sequentially
    mapM_ executeCmd $ [c | CodeLens{_command = Just c} <- codeLenses]

    edited <- replaceUnicodeQuotes <$> documentContents doc
    -- liftIO $ T.putStrLn edited

    let expectedFile = evalPath </> input <.> "expected"

    liftIO $ do
        -- Write expected file if missing
        missingExpected <- not <$> doesFileExist expectedFile
        when missingExpected $ T.writeFile expectedFile edited

    expected <- liftIO $ T.readFile expectedFile
    liftIO $ edited @?= expected

getEvalCodeLenses :: TextDocumentIdentifier -> Session [CodeLens]
getEvalCodeLenses = getCodeLensesBy isEvalTest

getCodeLensesBy :: (CodeLens -> Bool) -> TextDocumentIdentifier -> Session [CodeLens]
getCodeLensesBy f doc = filter f <$> getCodeLenses doc

-- Execute command and wait for result
executeCmd :: Command -> Session ()
executeCmd cmd = do
    executeCommand cmd
    _resp :: ApplyWorkspaceEditRequest <- skipManyTill anyMessage message
    -- liftIO $ print _resp
    return ()

-- Execute only Eval tests to avoid interference from other plugins (e.g ghcide:typesignature.add)
isEvalTest :: CodeLens -> Bool
isEvalTest (CodeLens _ (Just (Command _ cmd _)) _)
    | ":eval:" `T.isInfixOf` cmd = True
isEvalTest _ = False

replaceUnicodeQuotes :: T.Text -> T.Text
replaceUnicodeQuotes = T.replace "‘" "'" . T.replace "’" "'"

evalPath :: FilePath
evalPath = "test/testdata/eval"

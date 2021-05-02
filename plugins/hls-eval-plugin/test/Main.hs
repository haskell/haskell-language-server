{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

module Main (
    main,
) where

import           Control.Lens            (_Just, preview, view)
import           Control.Monad           (when)
import           Data.Aeson              (fromJSON)
import           Data.Aeson.Types        (Result (Success))
import           Data.List.Extra         (nubOrdOn)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import qualified Ide.Plugin.Eval         as Eval
import           Ide.Plugin.Eval.Types   (EvalParams (..))
import           Language.LSP.Types.Lens (command, range, title)
import           System.Directory        (doesFileExist)
import           System.FilePath         ((<.>), (</>))
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

evalPlugin :: PluginDescriptor IdeState
evalPlugin = Eval.descriptor "eval"

tests :: TestTree
tests =
    testGroup
        "eval"
        [ testCase "Produces Evaluate code lenses" $
            runSessionWithServer evalPlugin evalPath $ do
                doc <- openDoc "T1.hs" "haskell"
                lenses <- getEvalCodeLenses doc
                liftIO $ map (preview $ command . _Just . title) lenses @?= [Just "Evaluate..."]
        , testCase "Produces Refresh code lenses" $
            runSessionWithServer evalPlugin evalPath $ do
                doc <- openDoc "T2.hs" "haskell"
                lenses <- getEvalCodeLenses doc
                liftIO $ map (preview $ command . _Just . title) lenses @?= [Just "Refresh..."]
        , testCase "Code lenses have ranges" $
            runSessionWithServer evalPlugin evalPath $ do
                doc <- openDoc "T1.hs" "haskell"
                lenses <- getEvalCodeLenses doc
                liftIO $ map (view range) lenses @?= [Range (Position 4 0) (Position 5 0)]
        , testCase "Multi-line expressions have a multi-line range" $ do
            runSessionWithServer evalPlugin evalPath $ do
                doc <- openDoc "T3.hs" "haskell"
                lenses <- getEvalCodeLenses doc
                liftIO $ map (view range) lenses @?= [Range (Position 3 0) (Position 5 0)]
        , testCase "Executed expressions range covers only the expression" $ do
            runSessionWithServer evalPlugin evalPath $ do
                doc <- openDoc "T2.hs" "haskell"
                lenses <- getEvalCodeLenses doc
                liftIO $ map (view range) lenses @?= [Range (Position 4 0) (Position 5 0)]
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
            "Multi line comments, with the last test line ends without newline"
            $ goldenTest "TEndingMulti.hs"
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
          expectFailBecause "Unexplained but minor issue" $
            testCase "Setting language option TupleSections" $
                goldenTest "TLanguageOptionsTupleSections.hs"
        , testCase ":set accepts ghci flags" $
            goldenTest "TFlags.hs"
        , testCase "IO expressions are supported, stdout/stderr output is ignored" $
            goldenTest "TIO.hs"
        , testCase "Property checking" $ goldenTest "TProperty.hs"
        , testCase
            "Prelude has no special treatment, it is imported as stated in the module"
            $ goldenTest "TPrelude.hs"
        , testCase
            "Don't panic on {-# UNPACK #-} pragma"
            $ goldenTest "TUNPACK.hs"
        , testCase
            "Can handle eval inside nested comment properly"
            $ goldenTest "TNested.hs"
        , testCase "Test on last line insert results correctly" $ do
                let mdl = "TLastLine.hs"
                -- Write the test file, to make sure that it has no final line return
                writeFile (evalPath </> mdl) "module TLastLine where\n\n-- >>> take 3 [1..]"
                goldenTest mdl
            , testGroup "with preprocessors"
            [ knownBrokenInEnv [HostOS Windows, GhcVer GHC84, GhcVer GHC86]
                "CPP eval on Windows and/or GHC <= 8.6 fails for some reasons" $
              testCase "CPP support" $ goldenTest "TCPP.hs"
            , knownBrokenForGhcVersions [GHC84, GHC86]
                "Preprocessor known to fail on GHC <= 8.6"
                $ testCase "Literate Haskell Bird Style" $ goldenTest "TLHS.lhs"
            -- , testCase "Literate Haskell LaTeX Style" $ goldenTest "TLHSLateX.lhs"
            ]
        , testCase "Works with NoImplicitPrelude"
            $ goldenTest "TNoImplicitPrelude.hs"
        , testCase "Variable 'it' works"
            $ goldenTest "TIt.hs"
        ]

goldenTest :: FilePath -> IO ()
goldenTest = goldenTestBy isEvalTest

{- |Execute all CodeLens accepted by 'filter'
 Compare results with the contents of corresponding '.expected' file (and creates it, if missing)
-}
goldenTestBy :: (CodeLens -> Bool) -> FilePath -> IO ()
goldenTestBy fltr input = runSessionWithServer evalPlugin evalPath $ do
    doc <- openDoc input "haskell"

    -- Execute lenses backwards, to avoid affecting their position in the source file
    codeLenses <- reverse <$> getCodeLensesBy fltr doc
    -- liftIO $ print codeLenses

    -- Execute sequentially, nubbing elements to avoid
    -- evaluating the same section with multiple tests
    -- more than twice
    mapM_ executeCmd
        $ nubOrdOn actSectionId [c | CodeLens{_command = Just c} <- codeLenses]

    edited <- replaceUnicodeQuotes <$> documentContents doc
    -- liftIO $ T.putStrLn edited

    let expectedFile = input <.> "expected"

    liftIO $ do
        -- Write expected file if missing
        missingExpected <- not <$> doesFileExist expectedFile
        when missingExpected $ T.writeFile expectedFile edited
        expected <- T.readFile expectedFile
        edited @?= expected

actSectionId :: Command -> Int
actSectionId Command{_arguments = Just (List [fromJSON -> Success EvalParams{..}])} = evalId
actSectionId _ = error "Invalid CodeLens"

getEvalCodeLenses :: TextDocumentIdentifier -> Session [CodeLens]
getEvalCodeLenses = getCodeLensesBy isEvalTest

getCodeLensesBy :: (CodeLens -> Bool) -> TextDocumentIdentifier -> Session [CodeLens]
getCodeLensesBy f doc = filter f <$> getCodeLenses doc

-- Execute command and wait for result
executeCmd :: Command -> Session ()
executeCmd cmd = do
    executeCommand cmd
    _resp <- skipManyTill anyMessage (message SWorkspaceApplyEdit)
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
evalPath = "test" </> "testdata"

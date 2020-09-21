{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval
  ( tests
  )
where

import           Control.Applicative.Combinators (skipManyTill)
import           Control.Monad.IO.Class          (MonadIO (liftIO))
import qualified Data.Text.IO                    as T
import           Language.Haskell.LSP.Test
import           Language.Haskell.LSP.Types      (ApplyWorkspaceEditRequest, CodeLens (CodeLens, _command, _range),
                                                  Command (_title),
                                                  Position (..), Range (..))
import           System.FilePath
import           Test.Hls.Util
import           Test.Tasty
import           Test.Tasty.ExpectedFailure      (expectFailBecause)
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup
  "eval"
  [ testCase "Produces Evaluate code lenses" $ do
    runSession hieCommand fullCaps evalPath $ do
      doc    <- openDoc "T1.hs" "haskell"
      lenses <- getCodeLenses doc
      liftIO $ map (fmap _title . _command) lenses @?= [Just "Evaluate..."]
  , testCase "Produces Refresh code lenses" $ do
    runSession hieCommand fullCaps evalPath $ do
      doc    <- openDoc "T2.hs" "haskell"
      lenses <- getCodeLenses doc
      liftIO $ map (fmap _title . _command) lenses @?= [Just "Refresh..."]
  , testCase "Code lenses have ranges" $ do
    runSession hieCommand fullCaps evalPath $ do
      doc    <- openDoc "T1.hs" "haskell"
      lenses <- getCodeLenses doc
      liftIO $ map _range lenses @?= [Range (Position 4 0) (Position 4 15)]
  , testCase "Multi-line expressions have a multi-line range" $ do
    runSession hieCommand fullCaps evalPath $ do
      doc    <- openDoc "T3.hs" "haskell"
      lenses <- getCodeLenses doc
      liftIO $ map _range lenses @?= [Range (Position 3 0) (Position 4 15)]
  , testCase "Executed expressions range covers only the expression" $ do
    runSession hieCommand fullCaps evalPath $ do
      doc    <- openDoc "T2.hs" "haskell"
      lenses <- getCodeLenses doc
      liftIO $ map _range lenses @?= [Range (Position 4 0) (Position 4 15)]
  , testCase "Evaluation of expressions" $ goldenTest "T1.hs"
  , testCase "Reevaluation of expressions" $ goldenTest "T2.hs"
  , testCase "Evaluation of expressions w/ imports" $ goldenTest "T3.hs"
  , testCase "Evaluation of expressions w/ lets" $ goldenTest "T4.hs"
  , testCase "Refresh an evaluation" $ goldenTest "T5.hs"
  , testCase "Refresh an evaluation w/ lets" $ goldenTest "T6.hs"
  , testCase "Refresh a multiline evaluation" $ goldenTest "T7.hs"
  , testCase "Evaluate incorrect expressions" $ goldenTest "T8.hs"
  , testCase "Applies file LANGUAGE extensions" $ goldenTest "T9.hs"
  , testCase "Evaluate a type with :kind!" $ goldenTest "T10.hs"
  , testCase "Reports an error for an incorrect type with :kind!"
    $ goldenTest "T11.hs"
  , testCase "Shows a kind with :kind" $ goldenTest "T12.hs"
  , testCase "Reports an error for an incorrect type with :kind"
    $ goldenTest "T13.hs"
  , testCase "Returns a fully-instantiated type for :type"
    $ goldenTest "T14.hs"
  , testCase "Returns an uninstantiated type for :type +v, admitting multiple whitespaces around arguments"
    $ goldenTest "T15.hs"
  , testCase "Returns defaulted type for :type +d, admitting multiple whitespaces around arguments"
    $ goldenTest "T16.hs"
  , testCase ":type reports an error when given with unknown +x option"
    $ goldenTest "T17.hs"
  , testCase "Reports an error when given with unknown command"
    $ goldenTest "T18.hs"
  , testCase "Returns defaulted type for :type +d reflecting the default declaration specified in the >>> prompt"
    $ goldenTest "T19.hs"
  , expectFailBecause "known issue - see a note in P.R. #361"
  $ testCase ":type +d reflects the `default' declaration of the module"
  $ goldenTest "T20.hs"
  , testCase ":type handles a multilined result properly"
  $ goldenTest "T21.hs"
  , testCase ":t behaves exactly the same as :type"
  $ goldenTest "T22.hs"
  , testCase ":type does \"dovetails\" for short identifiers"
  $ goldenTest "T23.hs"
  , testCase ":kind! treats a multilined result properly"
  $ goldenTest "T24.hs"
  , testCase ":kind treats a multilined result properly"
  $ goldenTest "T25.hs"
  ]

goldenTest :: FilePath -> IO ()
goldenTest input = runSession hieCommand fullCaps evalPath $ do
  doc                              <- openDoc input "haskell"
  [CodeLens { _command = Just c }] <- getCodeLenses doc
  executeCommand c
  _resp :: ApplyWorkspaceEditRequest <- skipManyTill anyMessage message
  edited <- documentContents doc
  expected <- liftIO $ T.readFile $ evalPath </> input <.> "expected"
  liftIO $ edited @?= expected

evalPath :: FilePath
evalPath = "test/testdata/eval"

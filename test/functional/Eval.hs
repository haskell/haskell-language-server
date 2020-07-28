{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval
  ( tests
  )
where

import           Control.Applicative.Combinators
                                                ( skipManyTill )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import qualified Data.Text.IO                  as T
import           Language.Haskell.LSP.Test
import           Language.Haskell.LSP.Types     ( ApplyWorkspaceEditRequest
                                                , CodeLens
                                                  ( CodeLens
                                                  , _command
                                                  , _range
                                                  )
                                                , Command(_title)
                                                , Position(..)
                                                , Range(..)
                                                )
import           System.FilePath
import           Test.Hls.Util
import           Test.Tasty
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

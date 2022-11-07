{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE PatternSynonyms #-}

module Test.AddToWhere (tests) where

import Development.IDE
import Test.Tasty
import Language.LSP.Types (CodeAction(..))
import qualified Data.Text as T
import qualified Development.IDE.Plugin.CodeAction        as Refactor
import Test.Hls

import           Data.List.Extra

tests = testGroup "add to where" [
        mkGoldenAddArgTest "InsertNewWhere" (R 0 0 0 50),
        mkGoldenAddArgTest "PrependWhereDecls" (R 0 0 0 50),
        mkGoldenAddArgTest "PrependWhereDeclsComplex" (R 0 0 0 50)
  ]

mkGoldenAddArgTest :: FilePath -> Range -> TestTree
mkGoldenAddArgTest testFileName range = do
    let action docB = do
          _ <- waitForDiagnostics
          InR action@CodeAction {_title = actionTitle} : _ <-
            filter (\(InR CodeAction {_title = x}) -> "Add to" `isPrefixOf` T.unpack x)
              <$> getCodeActions docB range
          liftIO $ actionTitle @?= "Add to where ‘new_def’"
          executeCodeAction action
    goldenWithHaskellDoc
      (Refactor.bindingsPluginDescriptor mempty "ghcide-code-actions-bindings")
      (testFileName <> " (golden)")
      "test/data/golden/add_to_where"
      testFileName
      "expected"
      "hs"
      action

pattern R :: UInt -> UInt -> UInt -> UInt -> Range
pattern R x y x' y' = Range (Position x y) (Position x' y')

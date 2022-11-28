{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main
  ( main
  ) where

import           Control.Lens            (Prism', prism', (^.), (^..), (^?))
import           Control.Monad           (void)
import           Data.Maybe
import qualified Ide.Plugin.Class        as Class
import qualified Language.LSP.Types.Lens as J
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

classPlugin :: PluginTestDescriptor Class.Log
classPlugin = mkPluginTestDescriptor Class.descriptor "class"

tests :: TestTree
tests = testGroup
  "class"
  [codeActionTests, codeLensTests]

codeActionTests :: TestTree
codeActionTests = testGroup
  "code actions"
  [ testCase "Produces addMinimalMethodPlaceholders code actions for one instance" $ do
      runSessionWithServer classPlugin testDataDir $ do
        doc <- openDoc "T1.hs" "haskell"
        _ <- waitForDiagnosticsFromSource doc "typecheck"
        caResults <- getAllCodeActions doc
        liftIO $ map (^? _CACodeAction . J.title) caResults
          @?=
          [ Just "Add placeholders for '=='"
          , Just "Add placeholders for '==' with signature(s)"
          , Just "Add placeholders for '/='"
          , Just "Add placeholders for '/=' with signature(s)"
          ]
  , goldenWithClass "Creates a placeholder for '=='" "T1" "eq" $ \(eqAction:_) -> do
      executeCodeAction eqAction
  , goldenWithClass "Creates a placeholder for '/='" "T1" "ne" $ \(_:_:neAction:_) -> do
      executeCodeAction neAction
  , goldenWithClass "Creates a placeholder for 'fmap'" "T2" "fmap" $ \(_:_:_:_:fmapAction:_) -> do
      executeCodeAction fmapAction
  , goldenWithClass "Creates a placeholder for multiple methods 1" "T3" "1" $ \(mmAction:_) -> do
      executeCodeAction mmAction
  , goldenWithClass "Creates a placeholder for multiple methods 2" "T3" "2" $ \(_:_:mmAction:_) -> do
      executeCodeAction mmAction
  , goldenWithClass "Creates a placeholder for a method starting with '_'" "T4" "" $ \(_fAction:_) -> do
      executeCodeAction _fAction
  , goldenWithClass "Creates a placeholder for '==' with extra lines" "T5" "" $ \(eqAction:_) -> do
      executeCodeAction eqAction
  , goldenWithClass "Creates a placeholder for only the unimplemented methods of multiple methods" "T6" "1" $ \(gAction:_) -> do
      executeCodeAction gAction
  , goldenWithClass "Creates a placeholder for other two methods" "T6" "2" $ \(_:_:ghAction:_) -> do
      executeCodeAction ghAction
  , onlyRunForGhcVersions [GHC92, GHC94] "Only ghc-9.2+ enabled GHC2021 implicitly" $
      goldenWithClass "Don't insert pragma with GHC2021" "InsertWithGHC2021Enabled" "" $ \(_:eqWithSig:_) -> do
        executeCodeAction eqWithSig
  , goldenWithClass "Insert pragma if not exist" "InsertWithoutPragma" "" $ \(_:eqWithSig:_) -> do
      executeCodeAction eqWithSig
  , goldenWithClass "Don't insert pragma if exist" "InsertWithPragma" "" $ \(_:eqWithSig:_) -> do
      executeCodeAction eqWithSig
  , goldenWithClass "Only insert pragma once" "InsertPragmaOnce" "" $ \(_:multi:_) -> do
      executeCodeAction multi
  ]

codeLensTests :: TestTree
codeLensTests = testGroup
    "code lens"
    [ testCase "Has code lens" $ do
        runSessionWithServer classPlugin testDataDir $ do
            doc <- openDoc "CodeLensSimple.hs" "haskell"
            lens <- getCodeLenses doc
            let titles = map (^. J.title) $ mapMaybe (^. J.command) lens
            liftIO $ titles @?=
                [ "(==) :: B -> B -> Bool"
                , "(==) :: A -> A -> Bool"
                ]
    , goldenCodeLens "Apply code lens" "CodeLensSimple" 1
    , goldenCodeLens "Apply code lens for local class" "LocalClassDefine" 0
    , goldenCodeLens "Apply code lens on the same line" "Inline" 0
    , goldenCodeLens "Don't insert pragma while existing" "CodeLensWithPragma" 0
    , onlyRunForGhcVersions [GHC92, GHC94] "Only ghc-9.2+ enabled GHC2021 implicitly" $
        goldenCodeLens "Don't insert pragma while GHC2021 enabled" "CodeLensWithGHC2021" 0
    , goldenCodeLens "Qualified name" "Qualified" 0
    , goldenCodeLens "Type family" "TypeFamily" 0
    ]

_CACodeAction :: Prism' (Command |? CodeAction) CodeAction
_CACodeAction = prism' InR $ \case
  InR action -> Just action
  _          -> Nothing


goldenCodeLens :: TestName -> FilePath -> Int -> TestTree
goldenCodeLens title path idx =
    goldenWithHaskellDoc classPlugin title testDataDir path "expected" "hs" $ \doc -> do
        lens <- getCodeLenses doc
        executeCommand $ fromJust $ (lens !! idx) ^. J.command
        void $ skipManyTill anyMessage (message SWorkspaceApplyEdit)

goldenWithClass ::TestName -> FilePath -> FilePath -> ([CodeAction] -> Session ()) -> TestTree
goldenWithClass title path desc act =
  goldenWithHaskellDoc classPlugin title testDataDir path (desc <.> "expected") "hs" $ \doc -> do
    _ <- waitForDiagnosticsFromSource doc "typecheck"
    actions <- concatMap (^.. _CACodeAction) <$> getAllCodeActions doc
    act actions
    void $ skipManyTill anyMessage (getDocumentEdit doc)

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

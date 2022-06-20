{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main
  ( main
  ) where

import           Control.Lens                 (Prism', prism', (^..), (^?))
import           Control.Monad                (void)
import           Data.Functor.Contravariant   (contramap)
import           Development.IDE.Types.Logger
import qualified Ide.Plugin.Class             as Class
import qualified Language.LSP.Types.Lens      as J
import           System.FilePath
import           Test.Hls


main :: IO ()
main = do
    recorder <- makeDefaultStderrRecorder Nothing Debug
    defaultTestRunner . tests $ contramap (fmap pretty) recorder

classPlugin :: Recorder (WithPriority Class.Log) -> PluginDescriptor IdeState
classPlugin recorder = Class.descriptor recorder "class"

tests :: Recorder (WithPriority Class.Log) -> TestTree
tests recorder = testGroup
  "class"
  [ testCase "Produces addMinimalMethodPlaceholders code actions for one instance" $ do
      runSessionWithServer (classPlugin recorder) testDataDir $ do
        doc <- openDoc "T1.hs" "haskell"
        _ <- waitForDiagnosticsFromSource doc "typecheck"
        caResults <- getAllCodeActions doc
        liftIO $ map (^? _CACodeAction . J.title) caResults
          @?=
          [ Just "Add placeholders for '=='"
          , Just "Add placeholders for '/='"
          ]
  , goldenWithClass recorder "Creates a placeholder for '=='" "T1" "eq" $ \(eqAction:_) -> do
      executeCodeAction eqAction
  , goldenWithClass recorder "Creates a placeholder for '/='" "T1" "ne" $ \(_:neAction:_) -> do
      executeCodeAction neAction
  , goldenWithClass recorder "Creates a placeholder for 'fmap'" "T2" "fmap" $ \(_:_:fmapAction:_) -> do
      executeCodeAction fmapAction
  , goldenWithClass recorder "Creates a placeholder for multiple methods 1" "T3" "1" $ \(mmAction:_) -> do
      executeCodeAction mmAction
  , goldenWithClass recorder "Creates a placeholder for multiple methods 2" "T3" "2" $ \(_:mmAction:_) -> do
      executeCodeAction mmAction
  , goldenWithClass recorder "Creates a placeholder for a method starting with '_'" "T4" "" $ \(_fAction:_) -> do
      executeCodeAction _fAction
  , goldenWithClass recorder "Creates a placeholder for '==' with extra lines" "T5" "" $ \(eqAction:_) -> do
      executeCodeAction eqAction
  , goldenWithClass recorder "Creates a placeholder for only the unimplemented methods of multiple methods" "T6" "1" $ \(gAction:_) -> do
      executeCodeAction gAction
  , goldenWithClass recorder "Creates a placeholder for other two methods" "T6" "2" $ \(_:ghAction:_) -> do
      executeCodeAction ghAction
  ]

_CACodeAction :: Prism' (Command |? CodeAction) CodeAction
_CACodeAction = prism' InR $ \case
  InR action -> Just action
  _          -> Nothing

goldenWithClass :: Recorder (WithPriority Class.Log) -> TestName -> FilePath -> FilePath -> ([CodeAction] -> Session ()) -> TestTree
goldenWithClass recorder title path desc act =
  goldenWithHaskellDoc (classPlugin recorder) title testDataDir path (desc <.> "expected") "hs" $ \doc -> do
    _ <- waitForDiagnosticsFromSource doc "typecheck"
    actions <- concatMap (^.. _CACodeAction) <$> getAllCodeActions doc
    act actions
    void $ skipManyTill anyMessage (getDocumentEdit doc)

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main
  ( main
  ) where

import           Control.Monad         (void)
import qualified Ide.Plugin.ModuleName as ModuleName
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

moduleNamePlugin :: PluginTestDescriptor ModuleName.Log
moduleNamePlugin = mkPluginTestDescriptor ModuleName.descriptor "moduleName"

tests :: TestTree
tests =
  testGroup "moduleName"
  [ goldenWithModuleName "Add module header to empty module" "TEmptyModule" $ \doc -> do
      [CodeLens { _command = Just c }] <- getCodeLenses doc
      executeCommand c
      void $ skipManyTill anyMessage (message SWorkspaceApplyEdit)

  , goldenWithModuleName "Fix wrong module name" "TWrongModuleName" $ \doc -> do
      [CodeLens { _command = Just c }] <- getCodeLenses doc
      executeCommand c
      void $ skipManyTill anyMessage (message SWorkspaceApplyEdit)

  , goldenWithModuleName "Must infer module name as Main, if the file name starts with a lowercase" "mainlike" $ \doc -> do
      [CodeLens { _command = Just c }] <- getCodeLenses doc
      executeCommand c
      void $ skipManyTill anyMessage (message SWorkspaceApplyEdit)

  , goldenWithModuleName "Fix wrong module name in nested directory" "subdir/TWrongModuleName" $ \doc -> do
      [CodeLens { _command = Just c }] <- getCodeLenses doc
      executeCommand c
      void $ skipManyTill anyMessage (message SWorkspaceApplyEdit)
  , testCase "Should not show code lens if the module name is correct" $
      runSessionWithServer moduleNamePlugin testDataDir $ do
        doc <- openDoc "CorrectName.hs" "haskell"
        lenses <- getCodeLenses doc
        liftIO $ lenses @?= []
        closeDoc doc
  -- https://github.com/haskell/haskell-language-server/issues/3047
  , goldenWithModuleName "Fix#3047" "canonicalize/Lib/A" $ \doc -> do
      [CodeLens { _command = Just c }] <- getCodeLenses doc
      executeCommand c
      void $ skipManyTill anyMessage (message SWorkspaceApplyEdit)
  ]

goldenWithModuleName :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithModuleName title path = goldenWithHaskellDoc moduleNamePlugin title testDataDir path "expected" "hs"

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

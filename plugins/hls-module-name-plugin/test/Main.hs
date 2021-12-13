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

moduleNamePlugin :: PluginDescriptor IdeState
moduleNamePlugin = ModuleName.descriptor "moduleName"

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
  ]

goldenWithModuleName :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithModuleName title path = goldenWithHaskellDoc moduleNamePlugin title testDataDir path "expected" "hs"

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

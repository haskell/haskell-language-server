{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module ModuleName
  ( tests
  )
where

import qualified Data.Text.IO     as T
import           System.FilePath  ((<.>), (</>))
import           Test.Hls
import           Test.Hls.Command

tests :: TestTree
tests = testGroup
  "moduleName"
  [ testCase "Add module header to empty module" $ goldenTest "TEmptyModule.hs"
  , testCase "Fix wrong module name" $ goldenTest "TWrongModuleName.hs"
  , testCase "Must infer module name as Main, if the file name starts with a lowercase" $ goldenTest "mainlike.hs"
  ]

goldenTest :: FilePath -> IO ()
goldenTest input = runSession hlsCommand fullCaps testdataPath $ do
  doc                              <- openDoc input "haskell"
  -- getCodeLenses doc >>= liftIO . print . length
  [CodeLens { _command = Just c }] <- getCodeLenses doc
  executeCommand c
  _resp <- skipManyTill anyMessage (message SWorkspaceApplyEdit)
  edited <- documentContents doc
  -- liftIO $ T.writeFile (testdataPath </> input <.> "expected") edited
  expected <- liftIO $ T.readFile $ testdataPath </> input <.> "expected"
  liftIO $ edited @?= expected

testdataPath :: FilePath
testdataPath = "test/testdata/moduleName"

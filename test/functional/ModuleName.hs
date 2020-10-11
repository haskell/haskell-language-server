{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ModuleName
  ( tests
  )
where

import           Control.Applicative.Combinators
                                                ( skipManyTill )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import qualified Data.Text.IO                  as T
import           Language.Haskell.LSP.Test      ( fullCaps
                                                , documentContents
                                                , executeCommand
                                                , getCodeLenses
                                                , openDoc
                                                , runSession
                                                , anyMessage
                                                , message
                                                )
import           Language.Haskell.LSP.Types     ( ApplyWorkspaceEditRequest
                                                , CodeLens(..)
                                                )
import           System.FilePath                ( (<.>)
                                                , (</>)
                                                )
import           Test.Hls.Util                  ( hieCommand )
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( testCase
                                                , (@?=)
                                                )

tests :: TestTree
tests = testGroup
  "moduleName"
  [ testCase "Add module header to empty module" $ goldenTest "TEmptyModule.hs"
  , testCase "Fix wrong module name" $ goldenTest "TWrongModuleName.hs"
  ]

goldenTest :: FilePath -> IO ()
goldenTest input = runSession hieCommand fullCaps testdataPath $ do
  doc                              <- openDoc input "haskell"
  -- getCodeLenses doc >>= liftIO . print . length
  [CodeLens { _command = Just c }] <- getCodeLenses doc
  executeCommand c
  _resp :: ApplyWorkspaceEditRequest <- skipManyTill anyMessage message
  edited <- documentContents doc
  -- liftIO $ T.writeFile (testdataPath </> input <.> "expected") edited
  expected <- liftIO $ T.readFile $ testdataPath </> input <.> "expected"
  liftIO $ edited @?= expected

testdataPath :: FilePath
testdataPath = "test/testdata/moduleName"

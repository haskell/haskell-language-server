{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module RenameFile (
  renameFileTests,
) where

import           Control.Lens               ((^.))
import qualified Data.Text                  as T
import qualified Language.LSP.Protocol.Lens as L
import qualified System.FilePath            as FP
import           Test.Hls
import           Test.Hls.FileSystem
import           Util

renameVft :: [FileTree]
renameVft =
  [ directCradle ["Banana", "MyFile"]
  , copy "rename_file/Banana.hs"
  , copy "rename_file/MyFile.hs"
  ]

renameFileTests :: TestTree
renameFileTests =
  testGroup
    "rename file"
    [ testCase "banana Rename edits module declaration and imports" $ runRenameSession renameVft $ do
        let newName = "NewHaskell.hs"
        bananaDoc <- openDoc "Banana.hs" "haskell"
        content <- generateWorkspaceFileRenameTestSession "MyFile.hs" newName
        liftIO $ assertEqual "module header is renamed" content expectedModuleHeader
        bananaContent <- documentContents bananaDoc
        liftIO $ assertEqual "imports are renamed" bananaContent expectedImportsRenamed
   ]
 where
  generateWorkspaceFileRenameTestSession :: FilePath -> FilePath -> Session T.Text
  generateWorkspaceFileRenameTestSession haskellFile newFileName = do
    haskellDoc <- openDoc haskellFile "haskell"
    waitForAllProgressDone
    let fp =
          case uriToFilePath (haskellDoc ^. L.uri) of
            Just x -> x
            Nothing -> error "Could not parse uri to file path for: " <> haskellFile
    let haskellDir = FP.takeDirectory fp
        newId = mkTId $ haskellDir FP.</> newFileName
    renameFile haskellDoc newId
    newFile <- openDoc "NewHaskell.hs" "haskell"
    contents <- documentContents newFile
    pure contents

  expectedModuleHeader =
      [__i|
      module NewHaskell where

      foo = 3
      |]
  expectedImportsRenamed =
      [__i|
      module Banana where
      import NewHaskell (foo)
      import qualified NewHaskell
      import NewHaskell qualified
      import NewHaskell qualified as N
      bar = foo
      |]

  mkTId :: String -> TextDocumentIdentifier
  mkTId s = TextDocumentIdentifier $ filePathToUri s

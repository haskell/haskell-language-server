{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Workspace (
  cabalWorkspaceTests,
) where

import           Control.Lens                                  ((^.))
import           Data.String                                   (IsString (fromString))
import qualified Data.Text.Encoding                            as T
import           Distribution.PackageDescription               (ComponentName (..),
                                                                LibraryName (..),
                                                                PackageDescription,
                                                                benchmarkModules,
                                                                exeModules,
                                                                explicitLibModules,
                                                                foreignLibModules,
                                                                getComponent,
                                                                showComponentName,
                                                                testModules, Library (libName, exposedModules, libBuildInfo), Executable (buildInfo), BuildInfo (otherModules), TestSuite (testBuildInfo), Benchmark (benchmarkBuildInfo))
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.Types.Component                  (Component (..))
import           Ide.Plugin.Cabal.Parse                        (parseCabalFileContents)
import qualified Language.LSP.Protocol.Lens                    as L
import qualified System.FilePath                               as FP
import           Test.Hls
import           Utils

cabalWorkspaceTests :: TestTree
cabalWorkspaceTests =
  testGroup
    "Workspace"
    [ cabalRenameTests
    , cabalCreateTests
    ]

cabalRenameTests :: TestTree
cabalRenameTests =
  testGroup
    "Rename"
    [ runHaskellTestCaseSession "Rename in named library" "rename" $ do
        let newName = "NewHaskell.hs"
        pd <- generateWorkspaceFileRenameTestSession "rename.cabal" "LibLib.hs" newName
        checkModuleRenamedIn pd (FP.dropExtension newName) $ CLibName $ LSubLibName "lib"
    , runHaskellTestCaseSession "Rename in executable" "rename" $ do
        let newName = "ChangesModule.hs"
        pd <- generateWorkspaceFileRenameTestSession "rename.cabal" "Exe.hs" newName
        checkModuleRenamedIn pd (FP.dropExtension newName) $ CExeName "exe"
    , runHaskellTestCaseSession "Rename in main library" "rename" $ do
        let newName = "OtherNewHaskell.hs"
        pd <- generateWorkspaceFileRenameTestSession "rename.cabal" "lib/MainLib.hs" newName
        checkModuleRenamedIn pd (FP.dropExtension newName) $ CLibName LMainLibName
    , runHaskellTestCaseSession "Rename in benchmark" "rename" $ do
        let newName = "NewHaskell2.hs"
        pd <- generateWorkspaceFileRenameTestSession "rename.cabal" "Bench.hs" newName
        checkModuleRenamedIn pd (FP.dropExtension newName) $ CBenchName "bench"
    , runHaskellTestCaseSession "Rename in test-suite" "rename" $ do
        let newName = "NewHaskell.hs"
        pd <- generateWorkspaceFileRenameTestSession "rename.cabal" "Test.hs" newName
        checkModuleRenamedIn pd (FP.dropExtension newName) $ CTestName "test"
    ]
 where
  generateWorkspaceFileRenameTestSession :: FilePath -> FilePath -> FilePath -> Session PackageDescription
  generateWorkspaceFileRenameTestSession cabalFile haskellFile newFileName = do
    haskellDoc <- openDoc haskellFile "haskell"
    cabalDoc <- openDoc cabalFile "cabal"
    _ <- waitForDiagnosticsFrom haskellDoc
    let fp =
          case uriToFilePath (haskellDoc ^. L.uri) of
            Just x -> x
            Nothing -> error "Could not parse uri to file path for: " <> haskellFile
    let haskellDir = FP.takeDirectory fp
        newId = mkTId $ haskellDir FP.</> newFileName
    _ <- renameFile haskellDoc newId
    contents <- documentContents cabalDoc
    case parseCabalFileContents $ T.encodeUtf8 contents of
      (_, Right gpd) -> pure $ flattenPackageDescription gpd
      _ -> liftIO $ assertFailure "could not parse cabal file to gpd"

  mkTId :: String -> TextDocumentIdentifier
  mkTId s = TextDocumentIdentifier $ filePathToUri s

  checkModuleRenamedIn :: PackageDescription -> String -> ComponentName -> Session ()
  checkModuleRenamedIn pd newModName compName = do
    let comp = getComponent pd compName
        compModules = case comp of
          CLib lib     -> explicitLibModules lib
          CFLib fLib   -> foreignLibModules fLib
          CExe exe     -> exeModules exe
          CTest test   -> testModules test
          CBench bench -> benchmarkModules bench
        -- todo maybe check that old name is gone
        testDescription = newModName <> " was renamed in " <> showComponentName compName
    liftIO $ assertBool testDescription $ fromString newModName `elem` compModules


cabalCreateTests :: TestTree
cabalCreateTests =
  testGroup
    "Create"
    [ runHaskellTestCaseSession "Create in named library" "create" $ do
        let newName = "Lib.hs"
        pd <- generateWorkspaceFileCreateTestSession "lib-named/create.cabal" newName
        checkModuleCreateIn pd (FP.dropExtension newName) (CLibName $ LSubLibName "lib")
    , runHaskellTestCaseSession "Create in executable" "create" $ do
        let newName = "Exe.hs"
        pd <- generateWorkspaceFileCreateTestSession "exe/create.cabal" newName
        checkModuleCreateIn pd (FP.dropExtension newName) (CExeName "exe")
    , runHaskellTestCaseSession "Create in main library" "create" $ do
        let newName = "Lib.hs"
        pd <- generateWorkspaceFileCreateTestSession "lib/create.cabal" newName
        checkModuleCreateIn pd (FP.dropExtension newName) (CLibName LMainLibName)
    , runHaskellTestCaseSession "Create in benchmark" "create" $ do
        let newName = "Bench.hs"
        pd <- generateWorkspaceFileCreateTestSession "bench/create.cabal" newName
        checkModuleCreateIn pd (FP.dropExtension newName) (CBenchName "bench")
    , runHaskellTestCaseSession "Create in test-suite" "create" $ do
        let newName = "Test.hs"
        pd <- generateWorkspaceFileCreateTestSession "test/create.cabal" newName
        checkModuleCreateIn pd (FP.dropExtension newName) (CTestName "test")
    ]
 where
  generateWorkspaceFileCreateTestSession :: FilePath -> FilePath -> Session PackageDescription
  generateWorkspaceFileCreateTestSession cabalFile haskellFile = do
    cabalDoc <- openDoc cabalFile "cabal"
    _ <- createDoc haskellFile "haskell" "" -- empty file is fine
    contents <- documentContents cabalDoc
    case parseCabalFileContents $ T.encodeUtf8 contents of
      (_, Right gpd) -> pure $ flattenPackageDescription gpd
      _ -> liftIO $ assertFailure "could not parse cabal file to gpd"

  -- | tests if the new module is in `exposed-modules` for named libraries
  --   and `other-modules` for the other tests. Also asserts that we only
  --   have a single module per file
  checkModuleCreateIn :: PackageDescription -> String -> ComponentName -> Session ()
  checkModuleCreateIn pd newModName compName = do
    let comp = getComponent pd compName
    compModules <- case comp of
      CLib lib ->
        let name = libName lib in
        case name of
          LMainLibName  -> pure $ exposedModules lib -- exposed for libraries
          LSubLibName _ -> pure $ otherModules $ libBuildInfo lib
      -- CFLib fLib   -> pure $ foreignLibModules fLib
      CExe exe     -> pure $ otherModules $ buildInfo exe
      CTest test   -> pure $ otherModules $ testBuildInfo test
      CBench bench -> pure $ otherModules $ benchmarkBuildInfo bench
      _            -> liftIO $ assertFailure "unsupported module"
    let testDescription = newModName <> " was created in " <> showComponentName compName
    liftIO $ assertBool testDescription $ length compModules == 1 -- there should only be one module in the tests
    liftIO $ assertBool testDescription $ fromString newModName `elem` compModules

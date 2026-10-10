{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Util where
import           Data.Aeson          (KeyValue ((.=)))
import qualified Data.Map            as M
import qualified Data.Text           as T
import           Ide.Plugin.Config   (Config (..))
import qualified Ide.Plugin.Rename   as Rename
import           Ide.Types           (PluginConfig (..))
import           System.FilePath     ((<.>), (</>))
import           Test.Hls
import qualified Test.Hls.FileSystem as VFS
import           Test.Hls.FileSystem (FileTree, copy, directCradle)


projectDir :: [T.Text] -> [FileTree]
projectDir fps = directCradle fps : (map (copy . T.unpack) fps)

mkVFT :: [FileTree] -> VFS.VirtualFileTree
mkVFT = VFS.mkVirtualFileTree testDataDir

runRenameSession :: [VFS.FileTree] -> Session a -> IO a
runRenameSession subdir = failIfSessionTimeout
  .  runSessionWithTestConfig def
  { testDirLocation = Right $ mkVFT subdir
  , testPluginDescriptor = renamePlugin
  , testConfigCaps = codeActionNoResolveCaps }
  . const

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-rename-plugin" </> "test" </> "testdata"

renamePlugin :: PluginTestDescriptor Rename.Log
renamePlugin = mkPluginTestDescriptor Rename.descriptor "rename"

goldenWithModuleName :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithModuleName title path =
  goldenWithHaskellDocInTmpDir def renamePlugin title (mkVFT [directCradle [T.pack path], copy $ path <.> "hs", copy $ path <.> "expected" <.> "hs"]) modNameTestDataDir "expected" "hs"

modNameTestDataDir :: FilePath
modNameTestDataDir = testDataDir </> "mod_name"

goldenWithRename :: TestName-> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithRename title path act =
    goldenWithHaskellDoc (def { plugins = M.fromList [("rename", def { plcConfig = "crossModule" .= True })] })
       renamePlugin title testDataDir path "expected" "hs" act

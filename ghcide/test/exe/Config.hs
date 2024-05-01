{-# LANGUAGE PatternSynonyms #-}

module Config where

import           Ide.Types           (defaultPluginDescriptor)
import           System.FilePath     ((</>))
import           Test.Hls
import qualified Test.Hls.FileSystem as FS
import           Test.Hls.FileSystem (FileSystem)

testDataDir :: FilePath
testDataDir = "ghcide" </> "test" </> "data"

mkIdeTestFs :: [FS.FileTree] -> FS.VirtualFileTree
mkIdeTestFs = FS.mkVirtualFileTree testDataDir

-- * A dummy plugin for testing ghcIde
dummyPlugin :: PluginTestDescriptor ()
dummyPlugin = mkPluginTestDescriptor (\_ pid -> defaultPluginDescriptor pid "dummyTestPlugin") "core"

runWithDummyPlugin ::  FS.VirtualFileTree -> Session a -> IO a
runWithDummyPlugin = runSessionWithServerInTmpDir def dummyPlugin

runWithDummyPlugin' ::  FS.VirtualFileTree -> (FileSystem -> Session a) -> IO a
runWithDummyPlugin' = runSessionWithServerInTmpDirCont' def dummyPlugin

-- testSessionWithCorePlugin ::(TestRunner cont ()) => TestName -> FS.VirtualFileTree -> cont -> TestTree
testWithDummyPlugin :: String -> FS.VirtualFileTree -> Session () -> TestTree
testWithDummyPlugin caseName vfs = testCase caseName . runWithDummyPlugin vfs

testWithDummyPlugin' :: String -> FS.VirtualFileTree -> (FileSystem -> Session ()) -> TestTree
testWithDummyPlugin' caseName vfs = testCase caseName . runWithDummyPlugin' vfs

runWithDummyPluginEmpty :: Session a -> IO a
runWithDummyPluginEmpty = runWithDummyPlugin $ mkIdeTestFs []

testWithDummyPluginEmpty :: String -> Session () -> TestTree
testWithDummyPluginEmpty caseName = testWithDummyPlugin caseName $ mkIdeTestFs []

testWithDummyPluginEmpty' :: String -> (FileSystem -> Session ()) -> TestTree
testWithDummyPluginEmpty' caseName = testWithDummyPlugin' caseName $ mkIdeTestFs []

runWithExtraFiles :: String -> (FileSystem -> Session a) -> IO a
runWithExtraFiles dirName action = do
    let vfs = mkIdeTestFs [FS.copyDir dirName]
    runWithDummyPlugin' vfs action

testWithExtraFiles :: String -> String -> (FileSystem -> Session ()) -> TestTree
testWithExtraFiles testName dirName action = testCase testName $ runWithExtraFiles dirName action

pattern R :: UInt -> UInt -> UInt -> UInt -> Range
pattern R x y x' y' = Range (Position x y) (Position x' y')

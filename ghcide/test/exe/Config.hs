{-# LANGUAGE PatternSynonyms #-}

module Config where

import           Ide.Types           (defaultPluginDescriptor)
import           System.FilePath     ((</>))
import           Test.Hls
import qualified Test.Hls.FileSystem as FS

testDataDir :: FilePath
testDataDir = "ghcide" </> "test" </> "data"

mkIdeTestFs :: [FS.FileTree] -> FS.VirtualFileTree
mkIdeTestFs = FS.mkVirtualFileTree testDataDir

-- * A dummy plugin for testing ghcIde
dummyPlugin :: PluginTestDescriptor ()
dummyPlugin = mkPluginTestDescriptor (\_ pid -> defaultPluginDescriptor pid "dummyTestPlugin") "core"

runWithDummyPlugin :: FS.VirtualFileTree -> Session a -> IO a
runWithDummyPlugin = runSessionWithServerInTmpDir def dummyPlugin

pattern R :: UInt -> UInt -> UInt -> UInt -> Range
pattern R x y x' y' = Range (Position x y) (Position x' y')

module Config where
import           Ide.Types           (defaultPluginDescriptor)
import           System.FilePath     ((</>))
import           Test.Hls            (PluginTestDescriptor,
                                      mkPluginTestDescriptor)
import qualified Test.Hls.FileSystem as FS

testDataDir :: FilePath
testDataDir = "ghcide" </> "test" </> "data"

mkIdeTestFs :: [FS.FileTree] -> FS.VirtualFileTree
mkIdeTestFs = FS.mkVirtualFileTree testDataDir

-- * A dummy plugin for testing ghcIde
dummyPlugin :: PluginTestDescriptor ()
dummyPlugin = mkPluginTestDescriptor (\_ pid ->defaultPluginDescriptor pid "dummyTestPlugin") "core"

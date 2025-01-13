{-# LANGUAGE PatternSynonyms #-}

module Config(
    -- * basic config for ghcIde testing
    mkIdeTestFs
    , dummyPlugin

    -- * runners for testing specific plugins
    , testSessionWithPlugin
    -- * runners for testing with dummy plugin
    , runWithDummyPlugin
    , testWithDummyPlugin
    , testWithDummyPluginEmpty
    , testWithDummyPlugin'
    , testWithDummyPluginEmpty'
    , testWithConfig
    , testWithExtraFiles
    , runWithExtraFiles
    , runInDir
    , run

    -- * utilities for testing
    , Expect(..)
    , pattern R
    , mkR
    , checkDefs
    , mkL
    , withLongTimeout
    , lspTestCaps
    , lspTestCapsNoFileWatches
    ) where

import           Control.Exception           (bracket_)
import           Control.Lens.Setter         ((.~))
import           Control.Monad               (unless)
import           Data.Foldable               (traverse_)
import           Data.Function               ((&))
import qualified Data.Text                   as T
import           Development.IDE             (Pretty)
import           Development.IDE.Test        (canonicalizeUri)
import           Ide.Types                   (defaultPluginDescriptor)
import qualified Language.LSP.Protocol.Lens  as L
import           Language.LSP.Protocol.Types (Null (..))
import           System.Environment.Blank    (setEnv, unsetEnv)
import           System.FilePath             ((</>))
import           Test.Hls
import qualified Test.Hls.FileSystem         as FS

testDataDir :: FilePath
testDataDir = "ghcide" </> "test" </> "data"

mkIdeTestFs :: [FS.FileTree] -> FS.VirtualFileTree
mkIdeTestFs = FS.mkVirtualFileTree testDataDir

-- * Run with some injected plugin
-- testSessionWithPlugin ::  FS.VirtualFileTree -> (FilePath -> Session a) -> IO a
testSessionWithPlugin :: Pretty b => FS.VirtualFileTree -> PluginTestDescriptor b -> (FilePath -> Session a) -> IO a
testSessionWithPlugin fs plugin = runSessionWithTestConfig def
    { testPluginDescriptor = plugin
    , testDirLocation = Right fs
    , testConfigCaps = lspTestCaps
    , testShiftRoot = True
    }

-- * A dummy plugin for testing ghcIde
dummyPlugin :: PluginTestDescriptor ()
dummyPlugin = mkPluginTestDescriptor (\_ pid -> defaultPluginDescriptor pid "dummyTestPlugin") "core"

runWithDummyPlugin ::  FS.VirtualFileTree -> Session a -> IO a
runWithDummyPlugin = runSessionWithServerInTmpDir def dummyPlugin

testWithConfig :: String -> TestConfig () -> Session () -> TestTree
testWithConfig name conf s = testCase name $ runSessionWithTestConfig conf $ const s

runWithDummyPlugin' ::  FS.VirtualFileTree -> (FilePath -> Session a) -> IO a
runWithDummyPlugin' fs = runSessionWithTestConfig def
    { testPluginDescriptor = dummyPlugin
    , testDirLocation = Right fs
    , testConfigCaps = lspTestCaps
    , testShiftRoot = True
    }

testWithDummyPlugin :: String -> FS.VirtualFileTree -> Session () -> TestTree
testWithDummyPlugin caseName vfs = testWithDummyPlugin' caseName vfs . const

testWithDummyPlugin' :: String -> FS.VirtualFileTree -> (FilePath -> Session ()) -> TestTree
testWithDummyPlugin' caseName vfs = testCase caseName . runWithDummyPlugin' vfs

testWithDummyPluginEmpty :: String -> Session () -> TestTree
testWithDummyPluginEmpty caseName = testWithDummyPlugin caseName $ mkIdeTestFs []

testWithDummyPluginEmpty' :: String -> (FilePath -> Session ()) -> TestTree
testWithDummyPluginEmpty' caseName = testWithDummyPlugin' caseName $ mkIdeTestFs []

runWithExtraFiles :: String -> (FilePath -> Session a) -> IO a
runWithExtraFiles dirName action = do
    let vfs = mkIdeTestFs [FS.copyDir dirName]
    runWithDummyPlugin' vfs action

testWithExtraFiles :: String -> String -> (FilePath -> Session ()) -> TestTree
testWithExtraFiles testName dirName action = testCase testName $ runWithExtraFiles dirName action

runInDir :: FilePath -> Session a -> IO a
runInDir fs = runSessionWithServer def dummyPlugin fs

run :: Session a -> IO a
run = runSessionWithTestConfig def
    { testDirLocation = Right (mkIdeTestFs [])
    , testPluginDescriptor = dummyPlugin }
    . const

pattern R :: UInt -> UInt -> UInt -> UInt -> Range
pattern R x y x' y' = Range (Position x y) (Position x' y')

data Expect
  = ExpectRange Range -- Both gotoDef and hover should report this range
  | ExpectRanges [Range] -- definition lookup with multiple results
  | ExpectLocation Location
--  | ExpectDefRange Range -- Only gotoDef should report this range
  | ExpectHoverRange Range -- Only hover should report this range
  | ExpectHoverText [T.Text] -- the hover message must contain these snippets
  | ExpectHoverExcludeText [T.Text] -- the hover message must _not_ contain these snippets
  | ExpectHoverTextRegex T.Text -- the hover message must match this pattern
  | ExpectExternFail -- definition lookup in other file expected to fail
  | ExpectNoDefinitions
  | ExpectNoImplementations
  | ExpectNoHover
--  | ExpectExtern -- TODO: as above, but expected to succeed: need some more info in here, once we have some working examples
  deriving Eq

mkR :: UInt -> UInt -> UInt -> UInt -> Expect
mkR startLine startColumn endLine endColumn = ExpectRange $ mkRange startLine startColumn endLine endColumn

mkL :: Uri -> UInt -> UInt -> UInt -> UInt -> Expect
mkL uri startLine startColumn endLine endColumn = ExpectLocation $ Location uri $ mkRange startLine startColumn endLine endColumn


checkDefs :: Definition |? ([DefinitionLink] |? Null) -> Session [Expect] -> Session ()
checkDefs (defToLocation -> defs) mkExpectations = traverse_ check =<< mkExpectations where
  check (ExpectRange expectedRange) = do
    def <- assertOneDefinitionFound defs
    assertRangeCorrect def expectedRange
  check (ExpectRanges ranges) =
    traverse_ (assertHasRange defs) ranges
  check (ExpectLocation expectedLocation) = do
    def <- assertOneDefinitionFound defs
    liftIO $ do
      canonActualLoc <- canonicalizeLocation def
      canonExpectedLoc <- canonicalizeLocation expectedLocation
      canonActualLoc @?= canonExpectedLoc
  check ExpectNoImplementations = do
    liftIO $ assertBool "Expecting no implementations" $ null defs
  check ExpectNoDefinitions = do
    liftIO $ assertBool "Expecting no definitions" $ null defs
  check ExpectExternFail = liftIO $ assertFailure "Expecting to fail to find in external file"
  check _ = pure () -- all other expectations not relevant to getDefinition

  assertOneDefinitionFound :: [Location] -> Session Location
  assertOneDefinitionFound [def] = pure def
  assertOneDefinitionFound xs = liftIO . assertFailure $ "Expecting exactly one definition, got " <> show (length xs)

  assertRangeCorrect Location{_range = foundRange} expectedRange =
    liftIO $ expectedRange @=? foundRange

  assertHasRange actualRanges expectedRange = do
    let hasRange = any (\Location{_range=foundRange} -> foundRange == expectedRange) actualRanges
    unless hasRange $ liftIO $ assertFailure $
      "expected range: " <> show expectedRange <> "\nbut got ranges: " <> show defs

canonicalizeLocation :: Location -> IO Location
canonicalizeLocation (Location uri range) = Location <$> canonicalizeUri uri <*> pure range

defToLocation :: Definition |? ([DefinitionLink] |? Null) -> [Location]
defToLocation (InL (Definition (InL l))) = [l]
defToLocation (InL (Definition (InR ls))) = ls
defToLocation (InR (InL defLink)) = (\(DefinitionLink LocationLink{_targetUri,_targetRange}) -> Location _targetUri _targetRange) <$> defLink
defToLocation (InR (InR Null)) = []

lspTestCaps :: ClientCapabilities
lspTestCaps = fullLatestClientCaps { _window = Just $ WindowClientCapabilities (Just True) Nothing Nothing }

lspTestCapsNoFileWatches :: ClientCapabilities
lspTestCapsNoFileWatches = lspTestCaps & L.workspace . traverse . L.didChangeWatchedFiles .~ Nothing

withLongTimeout :: IO a -> IO a
withLongTimeout = bracket_ (setEnv "LSP_TIMEOUT" "120" True) (unsetEnv "LSP_TIMEOUT")

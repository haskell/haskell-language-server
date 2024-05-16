{-# LANGUAGE PatternSynonyms #-}

module Config(
    -- * basic config for ghcIde testing
    mkIdeTestFs
    , dummyPlugin

    -- * runners for testing with dummy plugin
    , runWithDummyPlugin
    , testWithDummyPlugin
    , testWithDummyPluginEmpty
    , testWithDummyPlugin'
    , testWithDummyPluginEmpty'
    , runWithExtraFiles
    , runInDir
    , testWithExtraFiles

    -- * utilities for testing definition and hover
    , Expect(..)
    , pattern R
    , mkR
    , checkDefs
    , mkL
    , lspTestCaps
    , lspTestCapsNoFileWatches
    ) where

import           Control.Lens.Setter         ((.~))
import           Data.Foldable               (traverse_)
import           Data.Function               ((&))
import qualified Data.Text                   as T
import           Development.IDE.Test        (canonicalizeUri)
import           Ide.Types                   (defaultPluginDescriptor)
import qualified Language.LSP.Protocol.Lens  as L
import           Language.LSP.Protocol.Types (Null (..))
import           System.FilePath             ((</>))
import           Test.Hls
import qualified Test.Hls.FileSystem         as FS
import           Test.Hls.FileSystem         (FileSystem, fsRoot)

testDataDir :: FilePath
testDataDir = "ghcide" </> "test" </> "data"

mkIdeTestFs :: [FS.FileTree] -> FS.VirtualFileTree
mkIdeTestFs = FS.mkVirtualFileTree testDataDir

-- * A dummy plugin for testing ghcIde
dummyPlugin :: PluginTestDescriptor ()
dummyPlugin = mkPluginTestDescriptor (\_ pid -> defaultPluginDescriptor pid "dummyTestPlugin") "core"

runWithDummyPlugin ::  FS.VirtualFileTree -> Session a -> IO a
runWithDummyPlugin = runSessionWithServerInTmpDir def dummyPlugin

runWithDummyPlugin' ::  FS.VirtualFileTree -> (FilePath -> Session a) -> IO a
runWithDummyPlugin' = runSessionWithServerInTmpDirCont' def dummyPlugin

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

pattern R :: UInt -> UInt -> UInt -> UInt -> Range
pattern R x y x' y' = Range (Position x y) (Position x' y')

data Expect
  = ExpectRange Range -- Both gotoDef and hover should report this range
  | ExpectLocation Location
--  | ExpectDefRange Range -- Only gotoDef should report this range
  | ExpectHoverRange Range -- Only hover should report this range
  | ExpectHoverText [T.Text] -- the hover message must contain these snippets
  | ExpectHoverExcludeText [T.Text] -- the hover message must _not_ contain these snippets
  | ExpectHoverTextRegex T.Text -- the hover message must match this pattern
  | ExpectExternFail -- definition lookup in other file expected to fail
  | ExpectNoDefinitions
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
  check (ExpectLocation expectedLocation) = do
    def <- assertOneDefinitionFound defs
    liftIO $ do
      canonActualLoc <- canonicalizeLocation def
      canonExpectedLoc <- canonicalizeLocation expectedLocation
      canonActualLoc @?= canonExpectedLoc
  check ExpectNoDefinitions = do
    liftIO $ assertBool "Expecting no definitions" $ null defs
  check ExpectExternFail = liftIO $ assertFailure "Expecting to fail to find in external file"
  check _ = pure () -- all other expectations not relevant to getDefinition

  assertOneDefinitionFound :: [Location] -> Session Location
  assertOneDefinitionFound [def] = pure def
  assertOneDefinitionFound xs = liftIO . assertFailure $ "Expecting exactly one definition, got " <> show (length xs)

  assertRangeCorrect Location{_range = foundRange} expectedRange =
    liftIO $ expectedRange @=? foundRange


canonicalizeLocation :: Location -> IO Location
canonicalizeLocation (Location uri range) = Location <$> canonicalizeUri uri <*> pure range

defToLocation :: Definition |? ([DefinitionLink] |? Null) -> [Location]
defToLocation (InL (Definition (InL l))) = [l]
defToLocation (InL (Definition (InR ls))) = ls
defToLocation (InR (InL defLink)) = (\(DefinitionLink LocationLink{_targetUri,_targetRange}) -> Location _targetUri _targetRange) <$> defLink
defToLocation (InR (InR Null)) = []

lspTestCaps :: ClientCapabilities
lspTestCaps = fullCaps { _window = Just $ WindowClientCapabilities (Just True) Nothing Nothing }

lspTestCapsNoFileWatches :: ClientCapabilities
lspTestCapsNoFileWatches = lspTestCaps & L.workspace . traverse . L.didChangeWatchedFiles .~ Nothing

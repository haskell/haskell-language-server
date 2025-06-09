module ConfigSchema where


import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char                  (toLower)
import           System.FilePath            ((</>))
import           System.Process.Extra
import           Test.Hls
import           Test.Hls.Command

-- | Integration test to capture changes to the generated default config and the vscode schema.
--
-- Changes to the vscode schema need to be communicated to vscode-haskell plugin maintainers,
-- otherwise users can't make use of the new configurations.
--
-- In general, changes to the schema need to be done consciously when new plugin or features are added.
-- To fix a failing of these tests, review the change. If it is expected, accept the change via:
--
-- @
--   TASTY_PATTERN="generate schema" cabal test func-test --test-options=--accept
-- @
--
-- As changes need to be applied for all GHC version specific configs, you either need to run this command for each
-- GHC version that is affected by the config change, or manually add the change to all other golden config files.
-- Likely, the easiest way is to run CI and apply the generated diffs manually.
tests :: TestTree
tests = testGroup "generate schema"
  [ goldenGitDiff "vscode-extension-schema" (vscodeSchemaFp ghcVersion) $ do
      stdout <- readProcess hlsExeCommand ["vscode-extension-schema"] ""
      pure $ BS.pack stdout
  , goldenGitDiff "generate-default-config" (defaultConfigFp ghcVersion) $ do
      stdout <- readProcess hlsExeCommand ["generate-default-config"] ""
      pure $ BS.pack stdout
  , goldenGitDiff "plugins-custom-config-markdown-reference" (markdownReferenceFp ghcVersion) $ do
    stdout <- readProcess hlsExeCommand ["plugins-custom-config-markdown-reference"] ""
    pure $ BS.pack stdout
  ]

vscodeSchemaFp :: GhcVersion -> FilePath
vscodeSchemaFp ghcVer = "test" </> "testdata" </> "schema" </> prettyGhcVersion ghcVer </> vscodeSchemaJson

defaultConfigFp :: GhcVersion -> FilePath
defaultConfigFp ghcVer = "test" </> "testdata" </> "schema" </> prettyGhcVersion ghcVer </> generateDefaultConfigJson

markdownReferenceFp :: GhcVersion -> FilePath
markdownReferenceFp ghcVer = "test" </> "testdata" </> "schema" </> prettyGhcVersion ghcVer </> markdownReferenceMd

vscodeSchemaJson :: FilePath
vscodeSchemaJson = "vscode-extension-schema.golden.json"

generateDefaultConfigJson :: FilePath
generateDefaultConfigJson = "default-config.golden.json"

markdownReferenceMd :: FilePath
markdownReferenceMd = "markdown-reference.md"

prettyGhcVersion :: GhcVersion -> String
prettyGhcVersion ghcVer = map toLower (show ghcVer)

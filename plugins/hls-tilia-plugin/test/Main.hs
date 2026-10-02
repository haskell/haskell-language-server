{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import           Control.Lens                ((^.))
import           Data.Aeson
import qualified Data.Aeson.KeyMap           as KM
import           Data.Functor
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import           Ide.Plugin.Config
import qualified Ide.Plugin.Tilia            as Tilia
import qualified Language.LSP.Protocol.Lens  as L
import           Language.LSP.Protocol.Types
import           System.FilePath
import           Test.Hls
import           Test.Hls.FileSystem

main :: IO ()
main = defaultTestRunner tests

tiliaPlugin :: PluginTestDescriptor Tilia.LogEvent
tiliaPlugin = mkPluginTestDescriptor Tilia.descriptor "tilia"

tests :: TestTree
tests = testGroup "tilia" $
    [False, True] <&> \cli ->
    testGroup (if cli then "cli" else "lib")
      [ goldenWithTilia cli "formats correctly" "Tilia" "formatted" $ \doc ->
          formatDoc doc formattingOptions
      , goldenWithTilia cli "takes fixities from the modules a module imports" "Fixity" "formatted" $ \doc ->
          formatDoc doc formattingOptions
      , goldenWithTilia cli "leaves alone a module it declines" "Declined" "formatted" $ \doc ->
          formatDoc doc formattingOptions
      , testCase "says why a module could not be formatted" $
          runSessionWithServerInTmpDir (tiliaConfig cli) tiliaPlugin testDataTree $ do
              doc <- openDoc "FormatError.hs" "haskell"
              void waitForBuildQueue
              resp <- request SMethod_TextDocumentFormatting $
                  DocumentFormattingParams Nothing doc formattingOptions
              liftIO $ case resp ^. L.result of
                  Left err -> do
                      let msg = err ^. L.message
                      assertBool ("Expected the parse error, got: " <> T.unpack msg)
                          ("cannot parse" `T.isInfixOf` msg)
                  Right _ ->
                      assertFailure "Expected formatting to fail on unparsable file"
      , testCase "refuses to format a range" $
          runSessionWithServerInTmpDir (tiliaConfig cli) tiliaPlugin testDataTree $ do
              doc <- openDoc "Tilia.hs" "haskell"
              void waitForBuildQueue
              resp <- request SMethod_TextDocumentRangeFormatting $
                  DocumentRangeFormattingParams Nothing doc (Range (Position 3 0) (Position 5 0)) formattingOptions
              liftIO $ case resp ^. L.result of
                  Left err -> do
                      let msg = err ^. L.message
                      assertBool ("Expected a refusal, got: " <> T.unpack msg)
                          ("whole modules" `T.isInfixOf` msg)
                  Right _ ->
                      assertFailure "Expected range formatting to be refused"
      ]

goldenWithTilia :: Bool -> TestName -> FilePath -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithTilia cli title path desc =
  goldenWithHaskellDocFormatterInTmpDir def tiliaPlugin "tilia" (pluginConfig cli) title testDataTree path desc "hs"

tiliaConfig :: Bool -> Config
tiliaConfig cli = def
  { formattingProvider = "tilia"
  , plugins = M.fromList [("tilia", pluginConfig cli)]
  }

pluginConfig :: Bool -> PluginConfig
pluginConfig cli = def {plcConfig = KM.fromList ["external" .= cli]}

formattingOptions :: FormattingOptions
formattingOptions = FormattingOptions 4 True Nothing Nothing Nothing

-- | The test data copied into a directory of its own, with no project above
-- it, since Tilia solves a build plan for the project a module belongs to.
testDataTree :: VirtualFileTree
testDataTree = mkVirtualFileTree testDataDir [copyDir "."]

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-tilia-plugin" </> "test" </> "testdata"

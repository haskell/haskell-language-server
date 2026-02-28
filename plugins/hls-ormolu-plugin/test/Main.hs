{-# LANGUAGE CPP               #-}
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
import qualified Ide.Plugin.Ormolu           as Ormolu
import qualified Language.LSP.Protocol.Lens  as L
import           Language.LSP.Protocol.Types
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

ormoluPlugin :: PluginTestDescriptor Ormolu.LogEvent
ormoluPlugin = mkPluginTestDescriptor Ormolu.descriptor "ormolu"

tests :: TestTree
tests = testGroup "ormolu" $
    [False, True] <&> \cli ->
    testGroup (if cli then "cli" else "lib")
      [ goldenWithOrmolu cli "formats correctly" "Ormolu" "formatted" $ \doc -> do
          formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
      , goldenWithOrmolu cli "formats imports correctly" "Ormolu2" "formatted" $ \doc -> do
          formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
#if MIN_VERSION_ormolu(0,5,3)
      , goldenWithOrmolu cli "formats operators correctly" "Ormolu3" "formatted" $ \doc -> do
          formatDoc doc (FormattingOptions 4 True Nothing Nothing Nothing)
#endif
      , testCase "error message contains stderr output" $ do
          let cliConfig = def {
                  formattingProvider = "ormolu",
                  plugins = M.fromList [("ormolu", def { plcConfig = KM.fromList ["external" .= True] })]
              }
          runSessionWithServer cliConfig ormoluPlugin testDataDir $ do
              doc <- openDoc "FormatError.hs" "haskell"
              void waitForBuildQueue
              resp <- request SMethod_TextDocumentFormatting $
                  DocumentFormattingParams Nothing doc (FormattingOptions 4 True Nothing Nothing Nothing)
              liftIO $ case resp ^. L.result of
                  Left err -> do
                      let msg = err ^. L.message
                      -- Verify the error message structure:
                      -- 1. Contains the exit code prefix (base message intact)
                      assertBool ("Expected exit code prefix, got: " <> T.unpack msg)
                          ("failed with exit code" `T.isInfixOf` msg)
                      -- 2. Contains stderr parse error details
                      assertBool ("Expected parse error details from stderr, got: " <> T.unpack msg)
                          ("parse error" `T.isInfixOf` msg)
                  Right _ ->
                      assertFailure "Expected formatting to fail on invalid syntax file"
      ]

goldenWithOrmolu :: Bool -> TestName -> FilePath -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithOrmolu cli title path desc =
  goldenWithHaskellDocFormatter def ormoluPlugin "ormolu" conf title testDataDir path desc "hs"
 where
  conf = def{plcConfig = KM.fromList ["external" .= cli]}

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-ormolu-plugin" </> "test" </> "testdata"

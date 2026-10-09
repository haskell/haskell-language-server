{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import           Ide.Logger
import qualified Ide.Plugin.Cabal    as Cabal
import qualified Ide.Plugin.CabalFmt as CabalFmt
import           System.Directory    (findExecutable)
import           System.FilePath
import           Test.Hls

data TestLog
  = LogCabalFmt CabalFmt.Log
  | LogCabal Cabal.Log

instance Pretty TestLog where
  pretty = \case
    LogCabalFmt msg -> pretty msg
    LogCabal msg -> pretty msg

data CabalFmtFound = Found | NotFound

isTestIsolated :: Bool
#if hls_isolate_cabalfmt_tests
isTestIsolated = True
#else
isTestIsolated = False
#endif

isCabalFmtFound :: IO CabalFmtFound
isCabalFmtFound = case isTestIsolated of
  True -> pure Found
  False -> do
    cabalFmt <- findExecutable "cabal-fmt"
    pure $ maybe NotFound (const Found) cabalFmt

main :: IO ()
main = do
  foundCabalFmt <- isCabalFmtFound
  defaultTestRunner (tests foundCabalFmt)

cabalFmtPlugin :: PluginTestDescriptor TestLog
cabalFmtPlugin = mconcat
  [ mkPluginTestDescriptor (CabalFmt.descriptor . cmapWithPrio LogCabalFmt) "cabal-fmt"
  , mkPluginTestDescriptor (Cabal.descriptor . cmapWithPrio LogCabal) "cabal"
  ]

tests :: CabalFmtFound -> TestTree
tests found = testGroup "cabal-fmt"
  [ knownBrokenOnWindows "Eats newlines between comments" $
    cabalFmtGolden found "formats a simple document" "simple_testdata" "formatted_document" $ \doc -> do
      formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)

  -- TODO: cabal-fmt can't expand modules if .cabal file is read from stdin. Tracking
  -- issue: https://github.com/phadej/cabal-fmt/pull/82
  , cabalFmtGolden found "formats a document with expand:src comment" "commented_testdata" "formatted_document" $ \doc -> do
      formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)

  , cabalFmtGolden found "formats a document with lib information" "lib_testdata" "formatted_document" $ \doc -> do
      formatDoc doc (FormattingOptions 10 True Nothing Nothing Nothing)
  ]

cabalFmtGolden :: CabalFmtFound -> TestName -> FilePath -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
cabalFmtGolden NotFound title _ _ _ =
  testCase title $
    assertFailure $  "Couldn't find cabal-fmt on PATH or this is not an isolated run. "
                  <> "Use cabal flag 'isolateCabalFmtTests' to make it isolated or install cabal-fmt locally."
cabalFmtGolden Found title path desc act = goldenWithCabalDocFormatter def cabalFmtPlugin "cabal-fmt" conf title testDataDir path desc "cabal" act
  where
    conf = def

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-cabal-fmt-plugin" </> "test" </> "testdata"

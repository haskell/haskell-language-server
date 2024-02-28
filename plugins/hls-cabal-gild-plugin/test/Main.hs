{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import qualified Ide.Plugin.CabalGild as CabalGild
import           System.Directory     (findExecutable)
import           System.FilePath
import           Test.Hls

data CabalGildFound = Found | NotFound

isTestIsolated :: Bool
#if isolateTests
isTestIsolated = True
#else
isTestIsolated = False
#endif

isCabalFmtFound :: IO CabalGildFound
isCabalFmtFound = case isTestIsolated of
  True -> pure Found
  False-> do
    cabalGild <- findExecutable "cabal-gild"
    pure $ maybe NotFound (const Found) cabalGild

main :: IO ()
main = do
  foundCabalFmt <- isCabalFmtFound
  defaultTestRunner (tests foundCabalFmt)

cabalGildPlugin :: PluginTestDescriptor CabalGild.Log
cabalGildPlugin = mkPluginTestDescriptor CabalGild.descriptor "cabal-gild"

tests :: CabalGildFound -> TestTree
tests found = testGroup "cabal-gild"
  [ cabalGildGolden found "formats a simple document" "simple_testdata" "formatted_document" $ \doc -> do
      formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)

  , cabalGildGolden found "formats a document with expand:src comment" "commented_testdata" "formatted_document" $ \doc -> do
      formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)

  , cabalGildGolden found "formats a document with lib information" "lib_testdata" "formatted_document" $ \doc -> do
      formatDoc doc (FormattingOptions 10 True Nothing Nothing Nothing)
  ]

cabalGildGolden :: CabalGildFound -> TestName -> FilePath -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
cabalGildGolden NotFound title _ _ _ =
  testCase title $
    assertFailure $  "Couldn't find cabal-gild on PATH or this is not an isolated run. "
                  <> "Use cabal flag 'isolateTests' to make it isolated or install cabal-gild locally."
cabalGildGolden Found title path desc act = goldenWithCabalDocFormatter def cabalGildPlugin "cabal-gild" conf title testDataDir path desc "cabal" act
  where
    conf = def

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-cabal-gild-plugin" </> "test" </> "testdata"

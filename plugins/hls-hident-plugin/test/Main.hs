{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import qualified Ide.Plugin.HIndent as HIndent
import System.FilePath ( (</>) )
import Test.Hls
    ( testGroup,
      defaultTestRunner,
      TestName,
      TestTree,
      goldenWithHaskellDocFormatter,
      def,
      Session,
      TextDocumentIdentifier,
      formatDoc,
      formatRange,
      Position (..),
      Range (..),
      FormattingOptions (..),
     )

main :: IO ()
main = defaultTestRunner tests

tests :: TestTree
tests = testGroup "hindent" [
    goldenWithHIndent "formats a document" "HIndent" "formatted_document" $ \doc -> do
      formatDoc doc (FormattingOptions 2 True Nothing Nothing Nothing)
    -- formatting only first line
    , goldenWithHIndent "formats a range" "HIndent" "formatted_range" $ \doc -> do
      formatRange doc (FormattingOptions 2 True Nothing Nothing Nothing) (Range (Position 0 0) (Position 0 20))
  ]


goldenWithHIndent :: TestName -> FilePath -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithHIndent title fp desc = goldenWithHaskellDocFormatter (HIndent.descriptor "hindent") "hindent" def title testDataDir fp desc "hs"

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

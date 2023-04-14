{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Main ( main ) where

import           Data.Either                    (rights)
import qualified Data.Text                      as T
import qualified Ide.Plugin.OverloadedRecordDot as OverloadedRecordDot
import           System.FilePath                ((</>))
import           Test.Hls


main :: IO ()
main = defaultTestRunner test

plugin :: PluginTestDescriptor OverloadedRecordDot.Log
plugin = mkPluginTestDescriptor OverloadedRecordDot.descriptor "overloaded-record-dot"

test :: TestTree
test = testGroup "overloaded-record-dot"
  [ mkTest "Simple" "Simple" 10 8 10 16,
    mkTest "NoPragmaNeeded" "NoPragmaNeeded" 11 8 11 16,
    mkTest "NestedParens" "NestedParens" 15 8 15 13,
    mkTest "NestedDollar" "NestedDollar" 15 8 15 14
  ]

mkTest :: TestName -> FilePath -> UInt -> UInt -> UInt -> UInt -> TestTree
mkTest title fp x1 y1 x2 y2 =
  goldenWithHaskellDoc plugin title testDataDir fp "expected" "hs" $ \doc -> do
    (act:_) <- getExplicitFieldsActions doc x1 y1 x2 y2
    executeCodeAction act

getExplicitFieldsActions
  :: TextDocumentIdentifier
  -> UInt -> UInt -> UInt -> UInt
  -> Session [CodeAction]
getExplicitFieldsActions doc x1 y1 x2 y2 =
  findExplicitFieldsAction <$> getCodeActions doc range
  where
    range = Range (Position x1 y1) (Position x2 y2)

findExplicitFieldsAction :: [a |? CodeAction] -> [CodeAction]
findExplicitFieldsAction = filter isExplicitFieldsCodeAction . rights . map toEither

isExplicitFieldsCodeAction :: CodeAction -> Bool
isExplicitFieldsCodeAction CodeAction {_title} =
  "Convert to record dot syntax" `T.isPrefixOf` _title

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

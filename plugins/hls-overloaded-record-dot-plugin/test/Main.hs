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
  [ mkTest "Simple" "Simple" "name" 10 7 10 15,
    mkTest "NoPragmaNeeded" "NoPragmaNeeded" "name" 11 7 11 15,
    mkTest "NestedParens" "NestedParens" "name" 15 7 15 24,
    mkTest "NestedDot" "NestedDot" "name" 17 7 17 22,
    mkTest "NestedDollar" "NestedDollar" "name" 15 7 15 24,
    mkTest "MultilineCase" "MultilineCase" "name" 10 7 12 15,
    mkTest "Multiline" "Multiline" "name" 10 7 11 15
  ]

mkTest :: TestName -> FilePath -> T.Text -> UInt -> UInt -> UInt -> UInt -> TestTree
mkTest title fp selectorName x1 y1 x2 y2 =
  goldenWithHaskellDoc plugin title testDataDir fp "expected" "hs" $ \doc -> do
    (act:_) <- getExplicitFieldsActions doc selectorName x1 y1 x2 y2
    executeCodeAction act

getExplicitFieldsActions
  :: TextDocumentIdentifier
  -> T.Text
  -> UInt -> UInt -> UInt -> UInt
  -> Session [CodeAction]
getExplicitFieldsActions doc selectorName x1 y1 x2 y2 =
  findExplicitFieldsAction selectorName <$> getCodeActions doc range
  where
    range = Range (Position x1 y1) (Position x2 y2)

findExplicitFieldsAction :: T.Text  -> [a |? CodeAction] -> [CodeAction]
findExplicitFieldsAction selectorName = filter (isExplicitFieldsCodeAction selectorName) . rights . map toEither

isExplicitFieldsCodeAction :: T.Text -> CodeAction -> Bool
isExplicitFieldsCodeAction selectorName CodeAction {_title} =
  ("Convert `" <> selectorName <> "` to record dot syntax") `T.isPrefixOf` _title

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

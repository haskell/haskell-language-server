{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main ( main ) where

import           Data.Either               (rights)
import qualified Data.Text                 as T
import qualified Ide.Plugin.ExplicitFields as ExplicitFields
import           System.FilePath           ((<.>), (</>))
import           Test.Hls

main :: IO ()
main = defaultTestRunner test

plugin :: PluginTestDescriptor ExplicitFields.Log
plugin = mkPluginTestDescriptor ExplicitFields.descriptor "explicit-fields"

test :: TestTree
test = testGroup "explicit-fields"
  [ mkTest "WildcardOnly" "WildcardOnly" 12 10 12 20
  , mkTest "Unused" "Unused" 12 10 12 20
  , mkTest "Unused2" "Unused2" 12 10 12 20
  , mkTest "WithPun" "WithPun" 13 10 13 25
  , mkTest "WithExplicitBind" "WithExplicitBind" 12 10 12 32
  , mkTest "Mixed" "Mixed" 14 10 14 37
  , mkTest "Construction" "Construction" 16 5 16 15
  , mkTest "HsExpanded1" "HsExpanded1" 17 10 17 20
  , mkTest "HsExpanded2" "HsExpanded2" 23 10 23 22
  , mkTestNoAction "ExplicitBinds" "ExplicitBinds" 11 10 11 52
  , mkTestNoAction "Puns" "Puns" 12 10 12 31
  , mkTestNoAction "Infix" "Infix" 11 11 11 31
  , mkTestNoAction "Prefix" "Prefix" 10 11 10 28
  ]

mkTestNoAction :: TestName -> FilePath -> UInt -> UInt -> UInt -> UInt -> TestTree
mkTestNoAction title fp x1 y1 x2 y2 =
  testCase title $
    runSessionWithServer def plugin (testDataDir </> "noop") $ do
      doc <- openDoc (fp <.> "hs") "haskell"
      actions <- getExplicitFieldsActions doc x1 y1 x2 y2
      liftIO $ actions @?= []

mkTestWithCount :: Int -> TestName -> FilePath -> UInt -> UInt -> UInt -> UInt -> TestTree
mkTestWithCount cnt title fp x1 y1 x2 y2 =
  goldenWithHaskellAndCaps def codeActionResolveCaps plugin title testDataDir fp "expected" "hs" $ \doc -> do
    acts@(act:_) <- getExplicitFieldsActions doc x1 y1 x2 y2
    liftIO $ length acts @?= cnt
    executeCodeAction act

mkTest :: TestName -> FilePath -> UInt -> UInt -> UInt -> UInt -> TestTree
mkTest = mkTestWithCount 1

getExplicitFieldsActions
  :: TextDocumentIdentifier
  -> UInt -> UInt -> UInt -> UInt
  -> Session [CodeAction]
getExplicitFieldsActions doc x1 y1 x2 y2 =
  findExplicitFieldsAction <$> getAndResolveCodeActions doc range
  where
    range = Range (Position x1 y1) (Position x2 y2)

findExplicitFieldsAction :: [a |? CodeAction] -> [CodeAction]
findExplicitFieldsAction = filter isExplicitFieldsCodeAction . rights . map toEither

isExplicitFieldsCodeAction :: CodeAction -> Bool
isExplicitFieldsCodeAction CodeAction {_title} =
  "Expand record wildcard" `T.isPrefixOf` _title

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-explicit-record-fields-plugin" </> "test" </> "testdata"

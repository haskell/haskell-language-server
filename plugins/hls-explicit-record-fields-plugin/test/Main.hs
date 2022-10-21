{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Main ( main ) where

import           Data.Either               (rights)
import qualified Data.Text                 as T
import qualified Ide.Plugin.ExplicitFields as ExplicitFields
import           System.FilePath           ((<.>), (</>))
import           Test.Hls


main :: IO ()
main = defaultTestRunner test

plugin :: PluginDescriptor IdeState
plugin = ExplicitFields.descriptor mempty "explicit-fields"

test :: TestTree
test = testGroup "explicit-fields"
  [ mkTest "WildcardOnly" "WildcardOnly" 12 10 12 20
  , mkTest "WithPun" "WithPun" 13 10 13 25
  , mkTest "WithExplicitBind" "WithExplicitBind" 12 10 12 32
  , mkTest "Mixed" "Mixed" 13 10 13 37
  , mkTest "Construction" "Construction" 16 5 16 15
  , mkTestNoAction "ExplicitBinds" "ExplicitBinds" 11 10 11 52
  , mkTestNoAction "Puns" "Puns" 12 10 12 31
  , mkTestNoAction "Infix" "Infix" 11 11 11 31
  , mkTestNoAction "Prefix" "Prefix" 10 11 10 28
  ]

mkTestNoAction :: TestName -> FilePath -> UInt -> UInt -> UInt -> UInt -> TestTree
mkTestNoAction title fp x1 y1 x2 y2 =
  testCase title $
    runSessionWithServer plugin (testDataDir </> "noop") $ do
      doc <- openDoc (fp <.> "hs") "haskell"
      actions <- getExplicitFieldsActions doc x1 y1 x2 y2
      liftIO $ actions @?= []

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
  "Expand record wildcard" `T.isPrefixOf` _title

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

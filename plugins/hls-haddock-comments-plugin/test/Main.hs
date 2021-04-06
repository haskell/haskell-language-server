{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE ViewPatterns             #-}

module Main
  ( main,
  )
where

import qualified Data.ByteString.Lazy       as LBS
import           Data.Foldable              (find)
import           Data.Maybe                 (mapMaybe)
import           Data.Text                  (Text)
import           Data.Text.Encoding         (encodeUtf8)
import qualified Ide.Plugin.HaddockComments as HaddockComments
import           System.FilePath            ((<.>), (</>))
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

plugin :: PluginDescriptor IdeState
plugin = HaddockComments.descriptor "haddockComments"

tests :: TestTree
tests =
  testGroup
    "haddock comments"
    [ goldenTest "HigherRankFunction" Signature 4 6,
      goldenTest "KindSigFunction" Signature 9 10,
      goldenTest "MultivariateFunction" Signature 4 8,
      goldenTest "QualFunction" Signature 2 10,
      goldenTest "Record" Record 7 2,
      expectedNothing "ConstFunction" Signature 2 2,
      expectedNothing "StaleFunction" Signature 3 3,
      expectedNothing "StaleRecord" Record 3 12
    ]

goldenTest :: FilePath -> GenCommentsType -> Int -> Int -> TestTree
goldenTest fp (toTitle -> expectedTitle) l c = goldenGitDiff (fp <> " (golden)") goldenFilePath $
  runSessionWithServer plugin haddockCommentsPath $ do
    doc <- openDoc hsFilePath "haskell"
    actions <- getCodeActions doc (Range (Position l c) (Position l $ succ c))
    case find ((== Just expectedTitle) . caTitle) actions of
      Just (InR x) -> do
        executeCodeAction x
        LBS.fromStrict . encodeUtf8 <$> documentContents doc
      _ -> liftIO $ assertFailure "Unable to find CodeAction"
  where
    hsFilePath = fp <.> "hs"
    goldenFilePath = haddockCommentsPath </> fp <.> "expected" <.> "hs"

expectedNothing :: FilePath -> GenCommentsType -> Int -> Int -> TestTree
expectedNothing fp (toTitle -> expectedTitle) l c = testCase fp $
  runSessionWithServer plugin haddockCommentsPath $ do
    doc <- openDoc (fp <.> "hs") "haskell"
    titles <- mapMaybe caTitle <$> getCodeActions doc (Range (Position l c) (Position l $ succ c))
    liftIO $ expectedTitle `notElem` titles @? "Unexpected CodeAction"

data GenCommentsType = Signature | Record

toTitle :: GenCommentsType -> Text
toTitle Signature = "Generate signature comments"
toTitle Record    = "Generate fields comments"

caTitle :: (Command |? CodeAction) -> Maybe Text
caTitle (InR CodeAction {_title}) = Just _title
caTitle _                         = Nothing

haddockCommentsPath :: String
haddockCommentsPath = "test" </> "testdata"


{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE ViewPatterns             #-}

module Main
  ( main
  ) where

import           Data.Foldable              (find)
import           Data.Maybe                 (mapMaybe)
import           Data.Text                  (Text)
import qualified Ide.Plugin.HaddockComments as HaddockComments
import           System.FilePath            ((<.>), (</>))
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

haddockCommentsPlugin :: PluginTestDescriptor HaddockComments.Log
haddockCommentsPlugin = mkPluginTestDescriptor HaddockComments.descriptor "haddockComments"

tests :: TestTree
tests =
  testGroup
    "haddock comments"
    [ goldenWithHaddockComments "HigherRankFunction" Signature 4 6,
      goldenWithHaddockComments "KindSigFunction" Signature 9 10,
      goldenWithHaddockComments "MultivariateFunction" Signature 4 8,
      goldenWithHaddockComments "QualFunction" Signature 2 10,
      goldenWithHaddockComments "Record" Record 7 2,
      goldenWithHaddockComments "Record2" Record 3 6,
      goldenWithHaddockComments "InlineRecord" Record 3 20,
      expectedNothing "ConstFunction" Signature 2 2,
      expectedNothing "StaleFunction" Signature 3 3,
      expectedNothing "StaleRecord" Record 4 9
    ]

goldenWithHaddockComments :: FilePath -> GenCommentsType -> UInt -> UInt -> TestTree
goldenWithHaddockComments fp (toTitle -> expectedTitle) l c =
  goldenWithHaskellDoc haddockCommentsPlugin (fp <> " (golden)") testDataDir fp "expected" "hs" $ \doc -> do
    actions <- getCodeActions doc (Range (Position l c) (Position l $ succ c))
    case find ((== Just expectedTitle) . caTitle) actions of
      Just (InR x) -> executeCodeAction x
      _            -> liftIO $ assertFailure "Unable to find CodeAction"

expectedNothing :: FilePath -> GenCommentsType -> UInt -> UInt -> TestTree
expectedNothing fp (toTitle -> expectedTitle) l c = testCase fp $
  runSessionWithServer haddockCommentsPlugin testDataDir $ do
    doc <- openDoc (fp <.> "hs") "haskell"
    titles <- mapMaybe caTitle <$> getCodeActions doc (Range (Position l c) (Position l $ succ c))
    liftIO $ expectedTitle `notElem` titles @? "Unexpected CodeAction"

data GenCommentsType = Signature | Record

toTitle :: GenCommentsType -> Text
toTitle Signature = "Generate signature comments"
toTitle Record    = "Generate haddock comments"

caTitle :: (Command |? CodeAction) -> Maybe Text
caTitle (InR CodeAction {_title}) = Just _title
caTitle _                         = Nothing

testDataDir :: String
testDataDir = "test" </> "testdata"


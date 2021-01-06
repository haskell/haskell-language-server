{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module HaddockComments
  ( tests,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (find)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import System.FilePath ((<.>), (</>))
import Test.Hls.Util
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "haddock comments"
    [ normal "HigherRankFunction.hs" Signature 4 6,
      normal "KindSigFunction.hs" Signature 9 10,
      normal "MultivariateFunction.hs" Signature 2 8,
      normal "QualFunction.hs" Signature 2 10,
      normal "Record.hs" Record 7 2,
      expectedNothing "StaleFunction.hs" Signature 3 3,
      expectedNothing "StaleRecord.hs" Record 3 12
    ]

normal :: FilePath -> GenCommentsType -> Int -> Int -> TestTree
normal fp (toTitle -> expectedTitle) l c = testCase fp $
  runSession hlsCommand fullCaps haddockCommentsPath $ do
    doc <- openDoc fp "haskell"
    _ <- waitForDiagnostics
    actions <- getCodeActions doc (Range (Position l c) (Position l $ succ c))
    case find ((== Just expectedTitle) . caTitle) actions of
      Just (CACodeAction x) -> do
        executeCodeAction x
        contentAfterAction <- documentContents doc
        expected <- liftIO . T.readFile $ haddockCommentsPath </> fp <.> "expected"
        liftIO $ contentAfterAction @?= expected
      _ -> liftIO $ assertFailure "Unable to find CodeAction"

expectedNothing :: FilePath -> GenCommentsType -> Int -> Int -> TestTree
expectedNothing fp (toTitle -> expectedTitle) l c = testCase fp $
  runSession hlsCommand fullCaps haddockCommentsPath $ do
    doc <- openDoc fp "haskell"
    _ <- waitForDiagnostics
    titles <- mapMaybe caTitle <$> getCodeActions doc (Range (Position l c) (Position l $ succ c))
    liftIO $ expectedTitle `notElem` titles @? "Unexpected CodeAction"

data GenCommentsType = Signature | Record

toTitle :: GenCommentsType -> Text
toTitle Signature = "Generate signature comments"
toTitle Record = "Generate fields comments"

caTitle :: CAResult -> Maybe Text
caTitle (CACodeAction CodeAction {_title}) = Just _title
caTitle _ = Nothing

haddockCommentsPath :: String
haddockCommentsPath = "test/testdata/haddockComments"

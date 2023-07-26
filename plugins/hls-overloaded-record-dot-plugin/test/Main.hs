{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Main ( main ) where

import           Control.Lens                   ((^.))
import           Data.Either                    (rights)
import           Data.Functor                   (void)
import           Data.Maybe                     (isNothing)
import           Data.Row
import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as TL
import qualified Data.Text.Lazy.Encoding        as TL
import           Ide.Logger                     (Doc, Logger (Logger),
                                                 Pretty (pretty),
                                                 Priority (Debug),
                                                 Recorder (Recorder, logger_),
                                                 WithPriority (WithPriority, priority),
                                                 cfilter, cmapWithPrio,
                                                 makeDefaultStderrRecorder)
import qualified Ide.Plugin.OverloadedRecordDot as OverloadedRecordDot
import           Language.LSP.Protocol.Lens     as L
import           System.FilePath                ((<.>), (</>))
import           Test.Hls
import           Test.Hls.Util                  (codeActionNoResolveCaps,
                                                 codeActionResolveCaps)

main :: IO ()
main =
  defaultTestRunner test

plugin :: PluginTestDescriptor OverloadedRecordDot.Log
plugin = mkPluginTestDescriptor OverloadedRecordDot.descriptor "overloaded-record-dot"

test :: TestTree
test = testGroup "overloaded-record-dot"
      (mkTest "Simple" "Simple" "name" 10 7 10 15
    <> mkTest "NoPragmaNeeded" "NoPragmaNeeded" "name" 11 7 11 15
    <> mkTest "NestedParens" "NestedParens" "name" 15 7 15 24
    <> mkTest "NestedDot" "NestedDot" "name" 17 7 17 22
    <> mkTest "NestedDollar" "NestedDollar" "name" 15 7 15 24
    <> mkTest "MultilineCase" "MultilineCase" "name" 10 7 12 15
    <> mkTest "Multiline" "Multiline" "name" 10 7 11 15
    <> mkTest "MultilineExpanded" "MultilineExpanded" "owner" 28 8 28 19)

mkTest :: TestName -> FilePath -> T.Text -> UInt -> UInt -> UInt -> UInt -> [TestTree]
mkTest title fp selectorName x1 y1 x2 y2 =
    [mkNoResolveTest (title <> " without resolve") fp selectorName x1 y1 x2 y2,
    mkResolveTest (title <> " with resolve") fp selectorName x1 y1 x2 y2]

mkNoResolveTest :: TestName -> FilePath -> T.Text -> UInt -> UInt -> UInt -> UInt -> TestTree
mkNoResolveTest title fp selectorName x1 y1 x2 y2 =
  goldenWithHaskellAndCaps codeActionNoResolveCaps plugin title testDataDir fp "expected" "hs" $ \doc -> do
    (act:_) <- getExplicitFieldsActions doc selectorName x1 y1 x2 y2
    executeCodeAction act

mkResolveTest :: TestName -> FilePath -> T.Text -> UInt -> UInt -> UInt -> UInt -> TestTree
mkResolveTest title fp selectorName x1 y1 x2 y2 =
  goldenWithHaskellAndCaps codeActionResolveCaps plugin title testDataDir fp "expected" "hs" $ \doc -> do
    ((Right act):_) <- getAndResolveExplicitFieldsActions doc selectorName x1 y1 x2 y2
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

getAndResolveExplicitFieldsActions
  :: TextDocumentIdentifier
  -> T.Text
  -> UInt -> UInt -> UInt -> UInt
  -> Session [Either ResponseError CodeAction]
getAndResolveExplicitFieldsActions doc selectorName x1 y1 x2 y2 = do
    actions <- findExplicitFieldsAction selectorName <$> getCodeActions doc range
    rsp <- mapM (request SMethod_CodeActionResolve) (filter (\x -> isNothing (x ^. L.edit)) actions)
    pure $ (^. L.result) <$> rsp

  where
    range = Range (Position x1 y1) (Position x2 y2)

findExplicitFieldsAction :: T.Text  -> [a |? CodeAction] -> [CodeAction]
findExplicitFieldsAction selectorName = filter (isExplicitFieldsCodeAction selectorName) . rights . map toEither

isExplicitFieldsCodeAction :: T.Text -> CodeAction -> Bool
isExplicitFieldsCodeAction selectorName CodeAction {_title} =
  ("Convert `" <> selectorName <> "` to record dot syntax") `T.isPrefixOf` _title

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

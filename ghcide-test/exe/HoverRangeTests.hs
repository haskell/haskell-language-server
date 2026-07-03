{-# LANGUAGE DataKinds #-}

-- | Tests for the @haskell/hoverRange@ custom request, which provides
-- hover information for the smallest expression enclosing a given range.
module HoverRangeTests (tests) where

import           Config
import           Control.Monad                 (void)
import           Control.Monad.IO.Class        (liftIO)
import qualified Data.Aeson                    as A
import           Data.Proxy                    (Proxy (..))
import qualified Data.Text                     as T
import           Hover                         (assertFoundIn)
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types   hiding
                                               (SemanticTokenAbsolute (..),
                                                SemanticTokenRelative (..),
                                                SemanticTokensEdit (..),
                                                mkRange)
import           Language.LSP.Test
import           Test.Hls                      (waitForTypecheck)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "hover range"
  [ testWithDummyPluginEmpty "type of selected expression" $ do
      doc <- createDoc "A.hs" "haskell" source
      void $ waitForTypecheck doc
      hover <- getHoverRange doc (R 2 11 2 19)
      (msg, mbRange) <- extractHover hover
      liftIO $ do
        assertFoundIn "_ :: Int" msg
        mbRange @?= Just (R 2 11 2 19)
  , testWithDummyPluginEmpty "multi-line selection" $ do
      doc <- createDoc "A.hs" "haskell" source
      void $ waitForTypecheck doc
      hover <- getHoverRange doc (R 4 6 6 18)
      (msg, mbRange) <- extractHover hover
      liftIO $ do
        assertFoundIn "_ :: Int" msg
        mbRange @?= Just (R 4 6 6 18)
  , testWithDummyPluginEmpty "empty range behaves like hover at a position" $ do
      doc <- createDoc "A.hs" "haskell" source
      void $ waitForTypecheck doc
      hover <- getHoverRange doc (R 2 12 2 12)
      (msg, _) <- extractHover hover
      liftIO $ assertFoundIn "negate" msg
  , testWithDummyPluginEmpty "null for a selection spanning multiple declarations" $ do
      doc <- createDoc "A.hs" "haskell" source
      void $ waitForTypecheck doc
      hover <- getHoverRange doc (R 1 0 4 14)
      liftIO $ hover @?= InR Null
  , testWithDummyPluginEmpty "no result for a range outside any expression" $ do
      doc <- createDoc "A.hs" "haskell" source
      void $ waitForTypecheck doc
      hover <- getHoverRange doc (R 100 0 100 5)
      liftIO $ hover @?= InR Null
  ]
  where
    source = T.unlines
      [ "module A where"           -- 0
      , "combined :: Int"          -- 1
      , "combined = negate 3 + 7"  -- 2
      , "g :: Int -> Int"          -- 3
      , "g x = if x > 0"           -- 4
      , "        then x + 1"       -- 5
      , "        else x - 1"       -- 6
      ]

getHoverRange :: TextDocumentIdentifier -> Range -> Session (Hover |? Null)
getHoverRange doc range = do
  resp <- request (SMethod_CustomMethod (Proxy @"haskell/hoverRange")) $
    A.object ["textDocument" A..= doc, "range" A..= range]
  case resp of
    TResponseMessage{_result = Left err} ->
      liftIO $ assertFailure $ "hoverRange request failed: " <> show err
    TResponseMessage{_result = Right value} -> case A.fromJSON value of
      A.Error err     -> liftIO $ assertFailure $ "hoverRange response decode failed: " <> err
      A.Success hover -> pure hover

extractHover :: Hover |? Null -> Session (T.Text, Maybe Range)
extractHover hover = case hover of
  InL (Hover (InL (MarkupContent _ msg)) mbRange) -> pure (msg, mbRange)
  other -> liftIO $ assertFailure $ "Unexpected hoverRange response: " <> show other

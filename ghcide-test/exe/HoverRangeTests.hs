{-# LANGUAGE DataKinds #-}

-- | Tests for the @haskell/hoverRange@ custom request, which provides
-- hover information for the smallest expression enclosing a given range.
module HoverRangeTests (tests) where

import           Config
import           Control.Monad                 (void)
import           Control.Monad.IO.Class        (liftIO)
import qualified Data.Aeson                    as A
import           Data.Foldable                 (for_, traverse_)
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
  [ testGroup "basic"
    [ hoverRangeTest basicSource "type of selected sub-expression"
        (R 2 11 2 19) (Just $ R 2 11 2 19) ["_ :: Int"]
    , hoverRangeTest basicSource "misaligned selection snaps to the enclosing expression"
        (R 2 13 2 21) (Just $ R 2 11 2 23) ["_ :: Int"]
    , hoverRangeTest basicSource "literal"
        (R 2 22 2 23) (Just $ R 2 22 2 23) ["_ :: Int"]
    , hoverRangeTest basicSource "selection exactly covering an identifier gives the rich hover"
        (R 2 11 2 17) Nothing ["negate ::", "Int -> Int"]
    , hoverRangeTest basicSource "operator"
        (R 2 20 2 21) Nothing ["Int -> Int -> Int"]
    , hoverRangeTest basicSource "multi-line selection"
        (R 4 6 6 18) (Just $ R 4 6 6 18) ["_ :: Int"]
    , hoverRangeTest basicSource "branch of an if expression"
        (R 5 13 5 18) (Just $ R 5 13 5 18) ["_ :: Int"]
    , hoverRangeTest basicSource "string literal"
        (R 8 11 8 19) Nothing ["_ :: String"]
    , hoverRangeTest basicSource "empty range behaves like hover at a position"
        (R 2 12 2 12) Nothing ["negate"]
    , hoverRangeNullTest basicSource "null for a selection spanning multiple declarations"
        (R 1 0 4 14)
    , hoverRangeNullTest basicSource "null for a range outside any expression"
        (R 100 0 100 5)
    ]
  , testGroup "GADTs and DataKinds"
    [ hoverRangeTest gadtSource "partially applied constructor is instantiated"
        (R 8 8 8 15) Nothing ["_ ::", "-> Vec ('Succ ('Succ ('Succ 'Zero))) Int"]
    , hoverRangeTest gadtSource "nested constructor application"
        (R 8 26 8 38) Nothing ["_ :: Vec ('Succ 'Zero) Int"]
    , hoverRangeTest gadtSource "existential hides the type-level index"
        (R 10 9 10 22) Nothing ["_ :: SomeVec Int"]
    ]
  ]

basicSource :: T.Text
basicSource = T.unlines
  [ "module A where"                       -- 0
  , "combined :: Int"                      -- 1
  , "combined = negate 3 + 7"              -- 2
  , "g :: Int -> Int"                      -- 3
  , "g x = if x > 0"                       -- 4
  , "        then x + 1"                   -- 5
  , "        else x - 1"                   -- 6
  , "greeting :: String"                   -- 7
  , "greeting = \"hello \" ++ \"world\""   -- 8
  ]

gadtSource :: T.Text
gadtSource = T.unlines
  [ "{-# LANGUAGE DataKinds, GADTs #-}"                     -- 0
  , "module A where"                                        -- 1
  , "data Nat = Zero | Succ Nat"                            -- 2
  , "data Vec (n :: Nat) a where"                           -- 3
  , "  VNil  :: Vec 'Zero a"                                -- 4
  , "  VCons :: a -> Vec n a -> Vec ('Succ n) a"            -- 5
  , "data SomeVec a where SomeVec :: Vec n a -> SomeVec a"  -- 6
  , "three :: Vec ('Succ ('Succ ('Succ 'Zero))) Int"        -- 7
  , "three = VCons 1 (VCons 2 (VCons 3 VNil))"              -- 8
  , "hidden :: SomeVec Int"                                 -- 9
  , "hidden = SomeVec three"                                -- 10
  ]

-- | Run @haskell/hoverRange@ over the given selection and check that the
-- hover text contains the given snippets and (optionally) that the reported
-- range is the enclosing expression's span.
hoverRangeTest :: T.Text -> TestName -> Range -> Maybe Range -> [T.Text] -> TestTree
hoverRangeTest src name sel expectedRange snippets =
  testWithDummyPluginEmpty name $ do
    doc <- createDoc "A.hs" "haskell" src
    void $ waitForTypecheck doc
    hover <- getHoverRange doc sel
    (msg, mbRange) <- extractHover hover
    liftIO $ do
      traverse_ (`assertFoundIn` msg) snippets
      for_ expectedRange $ \r -> mbRange @?= Just r

hoverRangeNullTest :: T.Text -> TestName -> Range -> TestTree
hoverRangeNullTest src name sel =
  testWithDummyPluginEmpty name $ do
    doc <- createDoc "A.hs" "haskell" src
    void $ waitForTypecheck doc
    hover <- getHoverRange doc sel
    liftIO $ hover @?= InR Null

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

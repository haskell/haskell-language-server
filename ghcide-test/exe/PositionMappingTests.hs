{-# LANGUAGE AllowAmbiguousTypes #-}

module PositionMappingTests (tests) where

import qualified Data.EnumMap.Strict                  as EM
import qualified Data.Text                            as T
import           Data.Text.Utf16.Rope.Mixed           (Rope)
import qualified Data.Text.Utf16.Rope.Mixed           as Rope
import           Development.IDE.Core.PositionMapping (PositionResult (..),
                                                       fromCurrent,
                                                       positionResultToMaybe,
                                                       toCurrent,
                                                       toCurrentPosition)
import           Development.IDE.Types.Location
import           Language.LSP.Protocol.Types          hiding
                                                      (SemanticTokenAbsolute (..),
                                                       SemanticTokenRelative (..),
                                                       SemanticTokensEdit (..),
                                                       mkRange)
import           Language.LSP.VFS                     (applyChange)
import           Test.QuickCheck
-- import Test.QuickCheck.Instances ()
import           Control.Arrow                        (second)
import           Data.Functor.Identity                (runIdentity)
import           Data.Text                            (Text)
import           Development.IDE.Core.Shake           (updatePositionMappingHelper)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

enumMapMappingTest :: TestTree
enumMapMappingTest = testCase "enumMapMappingTest" $ do
  let
      mkCE :: UInt -> UInt -> UInt -> UInt -> Text -> TextDocumentContentChangeEvent
      mkCE l1 c1 l2 c2 = mkChangeEvent (Range (Position l1 c1) (Position l2 c2))
      events :: [(Int32, [TextDocumentContentChangeEvent])]
      events = map (second return) [(0, mkCE 0 0 0 0 ""), (1, mkCE 0 1 0 1 " "), (2, mkCE 0 2 0 2 " "), (3, mkCE 0 3 0 3 " "), (4, mkCE 0 4 0 4 " "), (5, mkCE 0 5 0 5 " ")]
      finalMap = Prelude.foldl (\m (i, e) -> updatePositionMappingHelper i e m) mempty events
  let updatePose fromPos = do
        mapping <- snd <$> EM.lookup 0 finalMap
        toCurrentPosition mapping fromPos
  updatePose (Position 0 4) @?= Just (Position 0 9)
  updatePose (Position 0 5) @?= Just (Position 0 10)

mkChangeEvent :: Range -> Text -> TextDocumentContentChangeEvent
mkChangeEvent r t = TextDocumentContentChangeEvent $ InL
    TextDocumentContentChangePartial {_range = r, _rangeLength = Nothing, _text = t}

tests ::  TestTree
tests =
    testGroup "position mapping"
        [
        enumMapMappingTest
        , testGroup "toCurrent"
              [ testCase "before" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "ab"
                    (Position 0 0) @?= PositionExact (Position 0 0)
              , testCase "after, same line, same length" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "ab"
                    (Position 0 3) @?= PositionExact (Position 0 3)
              , testCase "after, same line, increased length" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc"
                    (Position 0 3) @?= PositionExact (Position 0 4)
              , testCase "after, same line, decreased length" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "a"
                    (Position 0 3) @?= PositionExact (Position 0 2)
              , testCase "after, next line, no newline" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc"
                    (Position 1 3) @?= PositionExact (Position 1 3)
              , testCase "after, next line, newline" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\ndef"
                    (Position 1 0) @?= PositionExact (Position 2 0)
              , testCase "after, same line, newline" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\nd"
                    (Position 0 4) @?= PositionExact (Position 1 2)
              , testCase "after, same line, newline + newline at end" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\nd\n"
                    (Position 0 4) @?= PositionExact (Position 2 1)
              , testCase "after, same line, newline + newline at end" $
                toCurrent
                    (Range (Position 0 1) (Position 0 1))
                    "abc"
                    (Position 0 1) @?= PositionExact (Position 0 4)
              ]
        , testGroup "fromCurrent"
              [ testCase "before" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "ab"
                    (Position 0 0) @?= PositionExact (Position 0 0)
              , testCase "after, same line, same length" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "ab"
                    (Position 0 3) @?= PositionExact (Position 0 3)
              , testCase "after, same line, increased length" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc"
                    (Position 0 4) @?= PositionExact (Position 0 3)
              , testCase "after, same line, decreased length" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "a"
                    (Position 0 2) @?= PositionExact (Position 0 3)
              , testCase "after, next line, no newline" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc"
                    (Position 1 3) @?= PositionExact (Position 1 3)
              , testCase "after, next line, newline" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\ndef"
                    (Position 2 0) @?= PositionExact (Position 1 0)
              , testCase "after, same line, newline" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\nd"
                    (Position 1 2) @?= PositionExact (Position 0 4)
              , testCase "after, same line, newline + newline at end" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\nd\n"
                    (Position 2 1) @?= PositionExact (Position 0 4)
              , testCase "after, same line, newline + newline at end" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 1))
                    "abc"
                    (Position 0 4) @?= PositionExact (Position 0 1)
              ]
        , adjustOption (\(QuickCheckTests i) -> QuickCheckTests (max 1000 i)) $ testGroup "properties"
              [ testProperty "fromCurrent r t <=< toCurrent r t" $ do
                -- Note that it is important to use suchThatMap on all values at once
                -- instead of only using it on the position. Otherwise you can get
                -- into situations where there is no position that can be mapped back
                -- for the edit which will result in QuickCheck looping forever.
                let gen = do
                        rope <- genRope
                        range <- genRange rope
                        PrintableText replacement <- arbitrary
                        oldPos <- genPosition rope
                        pure (range, replacement, oldPos)
                forAll
                    (suchThatMap gen
                        (\(range, replacement, oldPos) -> positionResultToMaybe $ (range, replacement, oldPos,) <$> toCurrent range replacement oldPos)) $
                    \(range, replacement, oldPos, newPos) ->
                    fromCurrent range replacement newPos === PositionExact oldPos
              , testProperty "toCurrent r t <=< fromCurrent r t" $ do
                let gen = do
                        rope <- genRope
                        range <- genRange rope
                        PrintableText replacement <- arbitrary
                        let newRope = runIdentity $ applyChange mempty rope $ mkChangeEvent range replacement
                        newPos <- genPosition newRope
                        pure (range, replacement, newPos)
                forAll
                    (suchThatMap gen
                        (\(range, replacement, newPos) -> positionResultToMaybe $ (range, replacement, newPos,) <$> fromCurrent range replacement newPos)) $
                    \(range, replacement, newPos, oldPos) ->
                    toCurrent range replacement oldPos === PositionExact newPos
              ]
        ]

newtype PrintableText = PrintableText { getPrintableText :: T.Text }
    deriving Show

instance Arbitrary PrintableText where
    arbitrary = PrintableText . T.pack . getPrintableString <$> arbitrary

genRope :: Gen Rope
genRope = Rope.fromText . getPrintableText <$> arbitrary

genPosition :: Rope -> Gen Position
genPosition r = do
    let rows :: Int = fromIntegral $ Rope.lengthInLines r
    row <- choose (0, max 0 $ rows - 1) `suchThat` inBounds @UInt
    let columns = T.length (nthLine (fromIntegral row) r)
    column <- choose (0, max 0 $ columns - 1) `suchThat` inBounds @UInt
    pure $ Position (fromIntegral row) (fromIntegral column)

genRange :: Rope -> Gen Range
genRange r = do
    let rows :: Int = fromIntegral $ Rope.lengthInLines r
    startPos@(Position startLine startColumn) <- genPosition r
    let maxLineDiff = max 0 $ rows - 1 - fromIntegral startLine
    endLine <- choose (fromIntegral startLine, fromIntegral startLine + maxLineDiff) `suchThat` inBounds @UInt
    let columns = T.length (nthLine (fromIntegral endLine) r)
    endColumn <-
        if fromIntegral startLine == endLine
            then choose (fromIntegral startColumn, columns)
            else choose (0, max 0 $ columns - 1)
        `suchThat` inBounds @UInt
    pure $ Range startPos (Position (fromIntegral endLine) (fromIntegral endColumn))

inBounds :: forall b a . (Integral a, Integral b, Bounded b) => a -> Bool
inBounds a = let i = toInteger a in i <= toInteger (maxBound @b) && i >= toInteger (minBound @b)

-- | Get the ith line of a rope, starting from 0. Trailing newline not included.
nthLine :: Int -> Rope -> T.Text
nthLine i r
    | Rope.null r = ""
    | otherwise = Rope.lines r !! i

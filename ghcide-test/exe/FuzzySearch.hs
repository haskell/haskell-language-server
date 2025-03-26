module FuzzySearch (tests) where

import           Data.Char                  (toLower)
import           Data.Maybe                 (catMaybes)
import qualified Data.Monoid.Textual        as T
import           Data.Text                  (Text, inits, pack)
import qualified Data.Text                  as Text
import           Prelude                    hiding (filter)
import           System.Directory           (doesFileExist)
import           System.IO.Unsafe           (unsafePerformIO)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.QuickCheck      (testProperty)
import qualified Text.Fuzzy                 as Fuzzy
import           Text.Fuzzy                 (Fuzzy (..))
import           Text.Fuzzy.Parallel

tests :: TestTree
tests =
  testGroup
    "Fuzzy search"
    [ needDictionary $
        testGroup
          "match works as expected on the english dictionary"
          [ testProperty "for legit words" propLegit,
            testProperty "for prefixes" propPrefix,
            testProperty "for typos" propTypo
          ]
    ]

test :: Text -> Bool
test candidate = do
  let previous =
        catMaybes
          [ (d,) . Fuzzy.score
              <$> referenceImplementation candidate d "" "" id
            | d <- dictionary
          ]
      new = catMaybes [(d,) <$> match candidate d | d <- dictionary]
  previous == new

propLegit :: Property
propLegit = forAll (elements dictionary) test

propPrefix :: Property
propPrefix = forAll (elements dictionary >>= elements . inits) test

propTypo :: Property
propTypo = forAll typoGen test

typoGen :: Gen Text
typoGen = do
  w <- elements dictionary
  l <- elements [0 .. Text.length w -1]
  let wl = Text.index w l
  c <- elements [ c | c <- ['a' .. 'z'], c /= wl]
  return $ replaceAt w l c

replaceAt :: Text -> Int -> Char -> Text
replaceAt t i c =
  let (l, r) = Text.splitAt i t
   in l <> Text.singleton c <> r

dictionaryPath :: FilePath
dictionaryPath = "/usr/share/dict/words"

{-# ANN dictionary ("HLint: ignore Avoid restricted function" :: String) #-}
{-# NOINLINE dictionary #-}
dictionary :: [Text]
dictionary = unsafePerformIO $ do
  existsDictionary <- doesFileExist dictionaryPath
  if existsDictionary
    then map pack . words <$> readFile dictionaryPath
    else pure []

referenceImplementation :: forall s t.
  (T.TextualMonoid s) =>
  -- | Pattern in lowercase except for first character
  s ->
  -- | The value containing the text to search in.
  t ->
  -- | The text to add before each match.
  s ->
  -- | The text to add after each match.
  s ->
  -- | The function to extract the text from the container.
  (t -> s) ->
  -- | The original value, rendered string and score.
  Maybe (Fuzzy t s)
referenceImplementation pat' t pre post extract =
  if null pat then Just (Fuzzy t result totalScore) else Nothing
  where
    null :: (T.TextualMonoid s) => s -> Bool
    null = not . T.any (const True)

    s = extract t
    (totalScore, _currScore, result, pat, _) =
      T.foldl'
        undefined
        ( \(tot, cur, res, pat, isFirst) c ->
            case T.splitCharacterPrefix pat of
              Nothing -> (tot, 0, res <> T.singleton c, pat, isFirst)
              Just (x, xs) ->
                -- the case of the first character has to match
                -- otherwise use lower case since the pattern is assumed lower
                let !c' = if isFirst then c else toLower c
                 in if x == c'
                      then
                        let cur' = cur * 2 + 1
                         in ( tot + cur',
                              cur',
                              res <> pre <> T.singleton c <> post,
                              xs,
                              False
                            )
                      else (tot, 0, res <> T.singleton c, pat, isFirst)
        )
        ( 0,
          1, -- matching at the start gives a bonus (cur = 1)
          mempty,
          pat',
          True
        )
        s

needDictionary :: TestTree -> TestTree
needDictionary
  | null dictionary = ignoreTestBecause ("not found: " <> dictionaryPath)
  | otherwise = id

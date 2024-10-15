{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}

module FindImplementationAndHoverTests (tests) where

import           Control.Monad
import           Data.Foldable
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Language.LSP.Protocol.Lens as L
import           Language.LSP.Test
import           Text.Regex.TDFA            ((=~))

import           Config
import           Control.Category           ((>>>))
import           Control.Lens               ((^.))
import           Development.IDE.Test       (standardizeQuotes)
import           Test.Hls
import           Test.Hls.FileSystem        (copyDir)

tests :: TestTree
tests = let
  tst :: (TextDocumentIdentifier -> Position -> Session a, a -> Session [Expect] -> Session ()) -> Position -> String -> Session [Expect] -> String -> TestTree
  tst (get, check) pos sfp targetRange title =
    testWithDummyPlugin title (mkIdeTestFs [copyDir "hover"]) $ do
        doc <- openDoc sfp "haskell"
        waitForProgressDone
        _x <- waitForTypecheck doc
        found <- get doc pos
        check found targetRange

  checkHover :: (HasCallStack) => Maybe Hover -> Session [Expect] -> Session ()
  checkHover hover expectations = traverse_ check =<< expectations where

    check :: (HasCallStack) => Expect -> Session ()
    check expected =
      case hover of
        Nothing -> unless (expected == ExpectNoHover) $ liftIO $ assertFailure "no hover found"
        Just Hover{_contents = (InL MarkupContent{_value = standardizeQuotes -> msg})
                  ,_range    = rangeInHover } ->
          case expected of
            ExpectRange  expectedRange -> checkHoverRange expectedRange rangeInHover msg
            ExpectHoverRange expectedRange -> checkHoverRange expectedRange rangeInHover msg
            ExpectHoverText snippets -> liftIO $ traverse_ (`assertFoundIn` msg) snippets
            ExpectHoverExcludeText snippets -> liftIO $ traverse_ (`assertNotFoundIn` msg) snippets
            ExpectHoverTextRegex re -> liftIO $ assertBool ("Regex not found in " <> T.unpack msg) (msg =~ re :: Bool)
            ExpectNoHover -> liftIO $ assertFailure $ "Expected no hover but got " <> show hover
            _ -> pure () -- all other expectations not relevant to hover
        _ -> liftIO $ assertFailure $ "test not expecting this kind of hover info" <> show hover

  extractLineColFromHoverMsg :: T.Text -> [T.Text]
  extractLineColFromHoverMsg =
    -- Hover messages contain multiple lines, and we are looking for the definition
    -- site
    T.lines
    -- The line we are looking for looks like: "*Defined at /tmp/GotoHover.hs:22:3*"
    -- So filter by the start of the line
    >>> mapMaybe (T.stripPrefix "*Defined at")
    -- There can be multiple definitions per hover message!
    -- See the test "field in record definition" for example.
    -- The tests check against the last line that contains the above line.
    >>> last
    -- [" /tmp/", "22:3*"]
    >>> T.splitOn (sourceFileName <> ":")
    -- "22:3*"
    >>> last
    -- ["22:3", ""]
    >>> T.splitOn "*"
    -- "22:3"
    >>> head
    -- ["22", "3"]
    >>> T.splitOn ":"

  checkHoverRange :: Range -> Maybe Range -> T.Text -> Session ()
  checkHoverRange expectedRange rangeInHover msg =
    let
      lineCol = extractLineColFromHoverMsg msg
      -- looks like hovers use 1-based numbering while definitions use 0-based
      -- turns out that they are stored 1-based in RealSrcLoc by GHC itself.
      adjust Position{_line = l, _character = c} =
        Position{_line = l + 1, _character = c + 1}
    in
    case map (read . T.unpack) lineCol of
      [l,c] -> liftIO $ adjust (expectedRange ^. L.start) @=? Position l c
      _     -> liftIO $ assertFailure $
        "expected: " <> show ("[...]" <> sourceFileName <> ":<LINE>:<COL>**[...]", Just expectedRange) <>
        "\n but got: " <> show (msg, rangeInHover)

  assertFoundIn :: T.Text -> T.Text -> Assertion
  assertFoundIn part whole = assertBool
    (T.unpack $ "failed to find: `" <> part <> "` in hover message:\n" <> whole)
    (part `T.isInfixOf` whole)

  assertNotFoundIn :: T.Text -> T.Text -> Assertion
  assertNotFoundIn part whole = assertBool
    (T.unpack $ "found unexpected: `" <> part <> "` in hover message:\n" <> whole)
    (not . T.isInfixOf part $ whole)

  sourceFilePath = T.unpack sourceFileName
  sourceFileName = "GotoImplementation.hs"

  mkFindTests tests = testGroup "goto implementation"
    [ testGroup "implementation" $ mapMaybe fst allTests
    , testGroup "hover"          $ mapMaybe snd allTests
    ]
    where
      allTests = tests ++ recordDotSyntaxTests

  recordDotSyntaxTests =
    -- We get neither new hover information nor 'Goto Implementation' locations for record-dot-syntax
    [ test' "RecordDotSyntax.hs" yes yes (Position 17 6)  [ExpectNoImplementations, ExpectHoverText ["_ :: [Char]"]] "hover over parent"
    , test' "RecordDotSyntax.hs" yes yes (Position 17 18) [ExpectNoImplementations, ExpectHoverText ["_ :: Integer"]] "hover over dot shows child"
    , test' "RecordDotSyntax.hs" yes yes (Position 17 25) [ExpectNoImplementations, ExpectHoverText ["_ :: MyChild"]] "hover over child"
    , test' "RecordDotSyntax.hs" yes yes (Position 17 27) [ExpectNoImplementations, ExpectHoverText ["_ :: [Char]"]] "hover over grandchild"
    ]

  test :: (HasCallStack) => (TestTree -> a) -> (TestTree -> b) -> Position -> [Expect] -> String -> (a, b)
  test runImpl runHover look expect = testM runImpl runHover look (return expect)

  testM :: (HasCallStack) => (TestTree -> a)
    -> (TestTree -> b)
    -> Position
    -> Session [Expect]
    -> String
    -> (a, b)
  testM = testM' sourceFilePath

  test' :: (HasCallStack) => FilePath -> (TestTree -> a) -> (TestTree -> b) -> Position -> [Expect] -> String -> (a, b)
  test' sourceFile runImpl runHover look expect = testM' sourceFile runImpl runHover look (return expect)

  testM' :: (HasCallStack)
    => FilePath
    -> (TestTree -> a)
    -> (TestTree -> b)
    -> Position
    -> Session [Expect]
    -> String
    -> (a, b)
  testM' sourceFile runImpl runHover look expect title =
    ( runImpl  $ tst impl  look sourceFile expect title
    , runHover $ tst hover look sourceFile expect title ) where
      impl  = (getImplementations, checkDefs)
      hover = (getHover          , checkHover)

  -- search locations            expectations on results
  -- TODO: Lookup of record field should return exactly one result
  aaaL = Position 8 15; aaaR = mkRange 5  9  5 16; aaa = [ExpectRanges [aaaR], ExpectHoverText ["Evidence of constraint 'Num AAA'", "bound by an instance of class Num"]]
  bbbL = Position 15 8; bbbR = mkRange 12 9 12 16; bbb = [ExpectRanges [bbbR], ExpectHoverText ["Evidence of constraint 'BBB AAA'", "bound by an instance of class BBB"]]
  in
  mkFindTests
  --      impl   hover   look       expect
  [
    test  yes    yes               aaaL      aaa           "locally defined class instance"
  , test  yes    yes               bbbL      bbb           "locally defined class and instance"
  ]
  where yes :: (TestTree -> Maybe TestTree)
        yes = Just -- test should run and pass
        no = const Nothing -- don't run this test at all

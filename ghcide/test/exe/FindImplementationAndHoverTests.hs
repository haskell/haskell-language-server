{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}

module FindImplementationAndHoverTests (tests) where

import           Control.Monad
import           Data.Foldable
import           Data.Maybe
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Language.LSP.Test
import           Text.Regex.TDFA      ((=~))

import           Config
import           Development.IDE.Test (standardizeQuotes)
import           Test.Hls
import           Test.Hls.FileSystem  (copyDir)

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
                  ,_range    = _rangeInHover } ->
          case expected of
            ExpectRange  _expectedRange -> liftIO $ assertFailure $ "ExpectRange assertion not implemented, yet."
            ExpectHoverRange _expectedRange -> liftIO $ assertFailure $ "ExpectHoverRange assertion not implemented, yet."
            ExpectHoverText snippets -> liftIO $ traverse_ (`assertFoundIn` msg) snippets
            ExpectHoverExcludeText snippets -> liftIO $ traverse_ (`assertNotFoundIn` msg) snippets
            ExpectHoverTextRegex re -> liftIO $ assertBool ("Regex not found in " <> T.unpack msg) (msg =~ re :: Bool)
            ExpectNoHover -> liftIO $ assertFailure $ "Expected no hover but got " <> show hover
            _ -> pure () -- all other expectations not relevant to hover
        _ -> liftIO $ assertFailure $ "test not expecting this kind of hover info" <> show hover

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

  aaaL = Position 8 15; aaaR = mkRange 5  9  5 16;
  aaa =
    [ ExpectRanges [aaaR]
    , ExpectHoverText (evidenceBoundByConstraint "Num" "AAA")
    ]

  bbbL = Position 15 8; bbbR = mkRange 12 9 12 16;
  bbb =
    [ ExpectRanges [bbbR]
    , ExpectHoverText (evidenceBoundByConstraint "BBB" "AAA")
    ]
  cccL = Position 18 11;
  ccc =
    [ ExpectNoImplementations
    , ExpectHoverText (evidenceBySignatureOrPattern "Show" "a")
    ]
  dddShowR = mkRange 21 26 21 30; dddEqR = mkRange 21 22 21 24
  dddL1 = Position 23 16;
  ddd1 =
    [ ExpectRanges [dddEqR]
    , ExpectHoverText
      [ constraintEvidence "Eq" "(Q k)"
      , evidenceGoal' "'forall k. Eq k => Eq (Q k)'"
      , boundByInstanceOf "Eq"
      , evidenceGoal "Eq" "k"
      , boundByTypeSigOrPattern
      ]
    ]
  dddL2 = Position 23 29;
  ddd2 =
    [ ExpectNoImplementations
    , ExpectHoverText (evidenceBySignatureOrPattern "Show" "k")
    ]
  dddL3 = Position 24 8;
  ddd3 =
    [ ExpectRanges [dddEqR, dddShowR]
    , ExpectHoverText
      [ constraintEvidence "Show" "(Q Integer)"
      , evidenceGoal' "'forall k. Show k => Show (Q k)'"
      , boundByInstance
      , evidenceGoal "Show" "Integer"
      , usingExternalInstance
      , constraintEvidence "Eq" "(Q Integer)"
      , evidenceGoal' "'forall k. Eq k => Eq (Q k)'"
      , boundByInstance
      , evidenceGoal "Eq" "Integer"
      , usingExternalInstance
      ]
    ]
  gadtL = Position 29 35;
  gadt =
    [ ExpectNoImplementations
    , ExpectHoverText
      [ constraintEvidence "Show" "Int"
      , evidenceGoal "Show" "a"
      , boundByTypeSigOrPattern
      , evidenceGoal' "'a ~ Int'"
      , boundByPattern
      ]
    ]
  in
  mkFindTests
  --      impl   hover   look       expect
  [
    test  yes    yes               aaaL      aaa           "locally defined class instance"
  , test  yes    yes               bbbL      bbb           "locally defined class and instance"
  , test  yes    yes               cccL      ccc           "bound by type signature"
  , test  yes    yes               dddL1     ddd1          "newtype Eq evidence"
  , test  yes    yes               dddL2     ddd2          "Show evidence"
  , test  yes    yes               dddL3     ddd3          "evidence construction"
  , test  yes    yes               gadtL     gadt          "GADT evidence"
  ]
  where yes :: (TestTree -> Maybe TestTree)
        yes = Just -- test should run and pass
        no = const Nothing -- don't run this test at all

-- ----------------------------------------------------------------------------
-- Helper functions for creating hover message verification
-- ----------------------------------------------------------------------------

evidenceBySignatureOrPattern :: Text -> Text -> [Text]
evidenceBySignatureOrPattern tyclass varname =
  [ constraintEvidence tyclass varname
  , boundByTypeSigOrPattern
  ]

evidenceBoundByConstraint :: Text -> Text -> [Text]
evidenceBoundByConstraint tyclass varname =
  [ constraintEvidence tyclass varname
  , boundByInstanceOf tyclass
  ]

boundByTypeSigOrPattern :: Text
boundByTypeSigOrPattern = "bound by type signature or pattern"

boundByInstance :: Text
boundByInstance =
  "bound by an instance of"

boundByInstanceOf :: Text -> Text
boundByInstanceOf tyvar =
  "bound by an instance of class " <> tyvar

boundByPattern :: Text
boundByPattern =
  "bound by a pattern"

usingExternalInstance :: Text
usingExternalInstance =
  "using an external instance"

constraintEvidence :: Text -> Text -> Text
constraintEvidence tyclass varname = "Evidence of constraint " <> quotedName tyclass varname

-- | A goal in the evidence tree.
evidenceGoal :: Text -> Text -> Text
evidenceGoal tyclass varname = "- " <> quotedName tyclass varname

evidenceGoal' :: Text -> Text
evidenceGoal' t = "- " <> t

quotedName :: Text -> Text -> Text
quotedName tyclass varname = "'" <> tyclass <> " " <> varname <> "'"

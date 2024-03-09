
module FindDefinitionAndHoverTests (tests) where

import           Control.Monad
import           Control.Monad.IO.Class         (liftIO)
import           Data.Foldable
import           Data.Maybe
import qualified Data.Text                      as T
import           Development.IDE.GHC.Compat     (GhcVersion (..), ghcVersion)
import           Development.IDE.GHC.Util
import           Development.IDE.Test           (expectDiagnostics,
                                                 standardizeQuotes)
import           Development.IDE.Types.Location
import qualified Language.LSP.Protocol.Lens     as L
import           Language.LSP.Protocol.Types    hiding
                                                (SemanticTokenAbsolute (..),
                                                 SemanticTokenRelative (..),
                                                 SemanticTokensEdit (..),
                                                 mkRange)
import           Language.LSP.Test
import           System.FilePath
import           System.Info.Extra              (isWindows)

import           Control.Lens                   ((^.))
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUtils
import           Text.Regex.TDFA                ((=~))

tests :: TestTree
tests = let

  tst :: (TextDocumentIdentifier -> Position -> Session a, a -> Session [Expect] -> Session ()) -> Position -> String -> Session [Expect] -> String -> TestTree
  tst (get, check) pos sfp targetRange title = testSessionWithExtraFiles "hover" title $ \dir -> do

    -- Dirty the cache to check that definitions work even in the presence of iface files
    liftIO $ runInDir dir $ do
      let fooPath = dir </> "Foo.hs"
      fooSource <- liftIO $ readFileUtf8 fooPath
      fooDoc <- createDoc fooPath "haskell" fooSource
      _ <- getHover fooDoc $ Position 4 3
      closeDoc fooDoc

    doc <- openTestDataDoc (dir </> sfp)
    waitForProgressDone
    found <- get doc pos
    check found targetRange



  checkHover :: Maybe Hover -> Session [Expect] -> Session ()
  checkHover hover expectations = traverse_ check =<< expectations where

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
  extractLineColFromHoverMsg = T.splitOn ":" . head . T.splitOn "*" . last . T.splitOn (sourceFileName <> ":")

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
  sourceFileName = "GotoHover.hs"

  mkFindTests tests = testGroup "get"
    [ testGroup "definition" $ mapMaybe fst tests
    , testGroup "hover"      $ mapMaybe snd tests
    , checkFileCompiles sourceFilePath $
        expectDiagnostics
          [ ( "GotoHover.hs", [(DiagnosticSeverity_Error, (62, 7), "Found hole: _")])
          , ( "GotoHover.hs", [(DiagnosticSeverity_Error, (65, 8), "Found hole: _")])
          ]
    , testGroup "type-definition" typeDefinitionTests
    , testGroup "hover-record-dot-syntax" recordDotSyntaxTests ]

  typeDefinitionTests = [ tst (getTypeDefinitions, checkDefs) aaaL14 sourceFilePath (pure tcData) "Saturated data con"
                        , tst (getTypeDefinitions, checkDefs) aL20 sourceFilePath (pure [ExpectNoDefinitions]) "Polymorphic variable"]

  recordDotSyntaxTests =
    [ tst (getHover, checkHover) (Position 17 24) (T.unpack "RecordDotSyntax.hs") (pure [ExpectHoverText ["x :: MyRecord"]]) "hover over parent"
    , tst (getHover, checkHover) (Position 17 25) (T.unpack "RecordDotSyntax.hs") (pure [ExpectHoverText ["_ :: MyChild"]]) "hover over dot shows child"
    , tst (getHover, checkHover) (Position 17 26) (T.unpack "RecordDotSyntax.hs") (pure [ExpectHoverText ["_ :: MyChild"]]) "hover over child"
    ]

  test runDef runHover look expect = testM runDef runHover look (return expect)

  testM runDef runHover look expect title =
    ( runDef   $ tst def   look sourceFilePath expect title
    , runHover $ tst hover look sourceFilePath expect title ) where
      def   = (getDefinitions, checkDefs)
      hover = (getHover      , checkHover)

  -- search locations            expectations on results
  fffL4  = fffR  ^. L.start;  fffR = mkRange 8  4    8  7 ; fff  = [ExpectRange fffR]
  fffL8  = Position 12  4  ;
  fffL14 = Position 18  7  ;
  aL20   = Position 19 15
  aaaL14 = Position 18 20  ;  aaa    = [mkR  11  0   11  3]
  dcL7   = Position 11 11  ;  tcDC   = [mkR   7 23    9 16]
  dcL12  = Position 16 11  ;
  xtcL5  = Position  9 11  ;  xtc    = [ExpectExternFail,   ExpectHoverText ["Int", "Defined in ", "GHC.Types", "ghc-prim"]]
  tcL6   = Position 10 11  ;  tcData = [mkR   7  0    9 16, ExpectHoverText ["TypeConstructor", "GotoHover.hs:8:1"]]
  vvL16  = Position 20 12  ;  vv     = [mkR  20  4   20  6]
  opL16  = Position 20 15  ;  op     = [mkR  21  2   21  4]
  opL18  = Position 22 22  ;  opp    = [mkR  22 13   22 17]
  aL18   = Position 22 20  ;  apmp   = [mkR  22 10   22 11]
  b'L19  = Position 23 13  ;  bp     = [mkR  23  6   23  7]
  xvL20  = Position 24  8  ;  xvMsg  = [ExpectExternFail,   ExpectHoverText ["pack", ":: String -> Text", "Data.Text", "text"]]
  clL23  = Position 27 11  ;  cls    = [mkR  25  0   26 20, ExpectHoverText ["MyClass", "GotoHover.hs:26:1"]]
  clL25  = Position 29  9
  eclL15 = Position 19  8  ;  ecls   = [ExpectExternFail, ExpectHoverText ["Num", "Defined in ", "GHC.Num", "base"]]
  dnbL29 = Position 33 18  ;  dnb    = [ExpectHoverText [":: ()"],   mkR  33 12   33 21]
  dnbL30 = Position 34 23
  lcbL33 = Position 37 26  ;  lcb    = [ExpectHoverText [":: Char"], mkR  37 26   37 27]
  lclL33 = Position 37 22
  mclL36 = Position 40  1  ;  mcl    = [mkR  40  0   40 14]
  mclL37 = Position 41  1
  spaceL37 = Position 41  24 ; space = [ExpectNoDefinitions, ExpectHoverText [":: Char"]]
  docL41 = Position 45  1  ;  doc    = [ExpectHoverText ["Recognizable docs: kpqz"]]
                           ;  constr = [ExpectHoverText ["Monad m"]]
  eitL40 = Position 44 28  ;  kindE  = [ExpectHoverText [":: Type -> Type -> Type\n"]]
  intL40 = Position 44 34  ;  kindI  = [ExpectHoverText [":: Type\n"]]
  tvrL40 = Position 44 37  ;  kindV  = [ExpectHoverText [":: * -> *\n"]]
  intL41 = Position 45 20  ;  litI   = [ExpectHoverText ["7518"]]
  chrL36 = Position 41 24  ;  litC   = [ExpectHoverText ["'f'"]]
  txtL8  = Position 12 14  ;  litT   = [ExpectHoverText ["\"dfgy\""]]
  lstL43 = Position 47 12  ;  litL   = [ExpectHoverText ["[8391 :: Int, 6268]"]]
  outL45 = Position 49  3  ;  outSig = [ExpectHoverText ["outer", "Bool"], mkR 50 0 50 5]
  innL48 = Position 52  5  ;  innSig = [ExpectHoverText ["inner", "Char"], mkR 49 2 49 7]
  holeL60 = Position 62 7  ;  hleInfo = [ExpectHoverText ["_ ::"]]
  holeL65 = Position 65 8  ;  hleInfo2 = [ExpectHoverText ["_ :: a -> Maybe a"]]
  cccL17 = Position 17 16  ;  docLink = [ExpectHoverTextRegex "\\*Defined in 'GHC.Types'\\* \\*\\(ghc-prim-[0-9.]+\\)\\*\n\n"]
  imported = Position 56 13 ; importedSig = getDocUri "Foo.hs" >>= \foo -> return [ExpectHoverText ["foo", "Foo", "Haddock"], mkL foo 5 0 5 3]
  reexported = Position 55 14 ; reexportedSig = getDocUri "Bar.hs" >>= \bar -> return [ExpectHoverText ["Bar", "Bar", "Haddock"], mkL bar 3 (if ghcVersion >= GHC94 then 5 else 0) 3 (if ghcVersion >= GHC94 then 8 else 14)]
  thLocL57 = Position 59 10 ; thLoc = [ExpectHoverText ["Identity"]]
  cmtL68 = Position 67  0  ;  lackOfdEq = [ExpectHoverExcludeText ["$dEq"]]
  import310 = Position 3 10; pkgTxt = [ExpectHoverText ["Data.Text\n\ntext-"]]
  in
  mkFindTests
  --      def    hover  look       expect
  [ -- It suggests either going to the constructor or to the field
    test  broken yes    fffL4      fff           "field in record definition"
  , test  yes    yes    fffL8      fff           "field in record construction    #1102"
  , test  yes    yes    fffL14     fff           "field name used as accessor"           -- https://github.com/haskell/ghcide/pull/120 in Calculate.hs
  , test  yes    yes    aaaL14     aaa           "top-level name"                        -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    dcL7       tcDC          "data constructor record         #1029"
  , test  yes    yes    dcL12      tcDC          "data constructor plain"                -- https://github.com/haskell/ghcide/pull/121
  , test  yes    yes    tcL6       tcData        "type constructor                #1028" -- https://github.com/haskell/ghcide/pull/147
  , test  broken yes    xtcL5      xtc           "type constructor external   #717,1028"
  , test  broken yes    xvL20      xvMsg         "value external package           #717" -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    vvL16      vv            "plain parameter"                       -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    aL18       apmp          "pattern match name"                    -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    opL16      op            "top-level operator               #713" -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    opL18      opp           "parameter operator"                    -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    b'L19      bp            "name in backticks"                     -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    clL23      cls           "class in instance declaration   #1027"
  , test  yes    yes    clL25      cls           "class in signature              #1027" -- https://github.com/haskell/ghcide/pull/147
  , test  broken yes    eclL15     ecls          "external class in signature #717,1027"
  , test  yes    yes    dnbL29     dnb           "do-notation   bind              #1073"
  , test  yes    yes    dnbL30     dnb           "do-notation lookup"
  , test  yes    yes    lcbL33     lcb           "listcomp   bind                 #1073"
  , test  yes    yes    lclL33     lcb           "listcomp lookup"
  , test  yes    yes    mclL36     mcl           "top-level fn 1st clause"
  , test  yes    yes    mclL37     mcl           "top-level fn 2nd clause         #1030"
  , test  yes    yes    spaceL37   space         "top-level fn on space           #1002"
  , test  no     yes    docL41     doc           "documentation                   #1129"
  , test  no     yes    eitL40     kindE         "kind of Either                  #1017"
  , test  no     yes    intL40     kindI         "kind of Int                     #1017"
  , test  no     broken tvrL40     kindV         "kind of (* -> *) type variable  #1017"
  , test  no     broken intL41     litI          "literal Int  in hover info      #1016"
  , test  no     broken chrL36     litC          "literal Char in hover info      #1016"
  , test  no     broken txtL8      litT          "literal Text in hover info      #1016"
  , test  no     broken lstL43     litL          "literal List in hover info      #1016"
  , test  yes    yes    cmtL68     lackOfdEq     "no Core symbols                 #3280"
  , test  no     yes    docL41     constr        "type constraint in hover info   #1012"
  , test  no     yes    outL45     outSig        "top-level signature              #767"
  , test  broken broken innL48     innSig        "inner     signature              #767"
  , test  no     yes    holeL60    hleInfo       "hole without internal name       #831"
  , test  no     yes    holeL65    hleInfo2      "hole with variable"
  , test  no     yes    cccL17     docLink       "Haddock html links"
  , testM yes    yes    imported   importedSig   "Imported symbol"
  , if isWindows then
        -- Flaky on Windows: https://github.com/haskell/haskell-language-server/issues/2997
        testM no     yes    reexported reexportedSig "Imported symbol (reexported)"
    else
        testM yes    yes    reexported reexportedSig "Imported symbol (reexported)"
  , test  no     yes       thLocL57   thLoc         "TH Splice Hover"
  , test yes yes import310 pkgTxt "show package name and its version"
  ]
  where yes, broken :: (TestTree -> Maybe TestTree)
        yes    = Just -- test should run and pass
        broken = Just . (`xfail` "known broken")
        no = const Nothing -- don't run this test at all
        --skip = const Nothing -- unreliable, don't run

checkFileCompiles :: FilePath -> Session () -> TestTree
checkFileCompiles fp diag =
  testSessionWithExtraFiles "hover" ("Does " ++ fp ++ " compile") $ \dir -> do
    void (openTestDataDoc (dir </> fp))
    diag

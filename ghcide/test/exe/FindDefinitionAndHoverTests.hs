{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}

module FindDefinitionAndHoverTests (tests) where

import           Control.Monad
import           Data.Foldable
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Language.LSP.Protocol.Lens as L
import           Language.LSP.Test
import           System.Info.Extra          (isWindows)

import           Config
import           Control.Category           ((>>>))
import           Control.Lens               ((^.))
import           Development.IDE.Test       (expectDiagnostics,
                                             standardizeQuotes)
import           Test.Hls
import           Test.Hls.FileSystem        (copyDir)
import           Text.Regex.TDFA            ((=~))

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
  sourceFileName = "GotoHover.hs"

  mkFindTests tests = testGroup "get"
    [ testGroup "definition" $ mapMaybe fst tests
    , testGroup "hover"      $ mapMaybe snd tests
    , testGroup "hover compile" [checkFileCompiles sourceFilePath $
        expectDiagnostics
          [ ( "GotoHover.hs", [(DiagnosticSeverity_Error, (62, 7), "Found hole: _", Just "GHC-88464")])
          , ( "GotoHover.hs", [(DiagnosticSeverity_Error, (65, 8), "Found hole: _", Just "GHC-88464")])
          ]]
    , testGroup "type-definition" typeDefinitionTests
    , testGroup "hover-record-dot-syntax" recordDotSyntaxTests ]

  typeDefinitionTests = [ tst (getTypeDefinitions, checkDefs) aaaL14 sourceFilePath (pure tcData) "Saturated data con"
                        , tst (getTypeDefinitions, checkDefs) aL20 sourceFilePath (pure [ExpectNoDefinitions]) "Polymorphic variable"]

  recordDotSyntaxTests =
    [ tst (getHover, checkHover) (Position 17 24) (T.unpack "RecordDotSyntax.hs") (pure [ExpectHoverText ["x :: MyRecord"]]) "hover over parent"
    , tst (getHover, checkHover) (Position 17 25) (T.unpack "RecordDotSyntax.hs") (pure [ExpectHoverText ["_ :: MyChild"]]) "hover over dot shows child"
    , tst (getHover, checkHover) (Position 17 26) (T.unpack "RecordDotSyntax.hs") (pure [ExpectHoverText ["_ :: MyChild"]]) "hover over child"
    ]

  test :: (HasCallStack) => (TestTree -> a) -> (TestTree -> b) -> Position -> [Expect] -> String -> (a, b)
  test runDef runHover look expect = testM runDef runHover look (return expect)

  testM :: (HasCallStack) => (TestTree -> a)
    -> (TestTree -> b)
    -> Position
    -> Session [Expect]
    -> String
    -> (a, b)
  testM runDef runHover look expect title =
    ( runDef   $ tst def   look sourceFilePath expect title
    , runHover $ tst hover look sourceFilePath expect title ) where
      def   = (getDefinitions, checkDefs)
      hover = (getHover      , checkHover)

  -- search locations            expectations on results
  -- TODO: Lookup of record field should return exactly one result
  fffL4  = fffR  ^. L.start;  fffR = mkRange 8  4    8  7; fff  = [ExpectRanges [fffR, mkRange 7 23 9 16]]
  fffL8  = Position 12  4  ;  fff' = [ExpectRange fffR]
  fffL14 = Position 18  7  ;
  aL20   = Position 19 15
  aaaL14 = Position 18 20  ;  aaa    = [mkR  11  0   11  3]
  dcL7   = Position 11 11  ;  tcDC   = [mkR   7 23    9 16]
  dcL12  = Position 16 11  ;
  xtcL5  = Position  9 11  ;  xtc    = [ExpectHoverText ["Int", "Defined in ", "GHC.Types", "ghc-prim"]]
  tcL6   = Position 10 11  ;  tcData = [mkR   7  0    9 16, ExpectHoverText ["TypeConstructor", "GotoHover.hs:8:1"]]
  vvL16  = Position 20 12  ;  vv     = [mkR  20  4   20  6]
  opL16  = Position 20 15  ;  op     = [mkR  21  2   21  4]
  opL18  = Position 22 22  ;  opp    = [mkR  22 13   22 17]
  aL18   = Position 22 20  ;  apmp   = [mkR  22 10   22 11]
  b'L19  = Position 23 13  ;  bp     = [mkR  23  6   23  7]
  xvL20  = Position 24  8  ;  xvMsg  = [ExpectHoverText ["pack", ":: String -> Text", "Data.Text", "text"]]
  clL23  = Position 27 11  ;  cls    = [mkR  25  0   26 20, ExpectHoverText ["MyClass", "GotoHover.hs:26:1"]]
  clL25  = Position 29  9
  eclL15 = Position 19  8  ;  ecls   = [ExpectHoverText ["Num", "Defined in ", if ghcVersion < GHC910 then "GHC.Num" else "GHC.Internal.Num", "base"]]
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
  -- TODO: Kind signature of type variables should be `Type -> Type`
  tvrL40 = Position 44 37  ;  kindV  = [ExpectHoverText ["m"]];        kindV' = [ExpectHoverText [":: * -> *\n"]]
  -- TODO: Hover of integer literal should be `7518`
  intL41 = Position 45 20  ;  litI   = [ExpectHoverText ["_ :: Int"]];  litI' = [ExpectHoverText ["7518"]]
  -- TODO: Hover info of char literal should be `'f'`
  chrL36 = Position 41 24  ;  litC   = [ExpectHoverText ["_ :: Char"]]; litC' = [ExpectHoverText ["'f'"]]
  -- TODO: Hover info of Text literal should be `"dfgy"`
  txtL8  = Position 12 14  ;  litT   = [ExpectHoverText ["_ :: Text"]]; litT' = [ExpectHoverText ["\"dfgy\""]]
  -- TODO: Hover info of List literal should be `[8391 :: Int, 6268]`
  lstL43 = Position 47 12  ;  litL   = [ExpectHoverText ["[Int]"]];     litL' = [ExpectHoverText ["[8391 :: Int, 6268]"]]
  outL45 = Position 49  3  ;  outSig = [ExpectHoverText ["outer", "Bool"], mkR 50 0 50 5]
  -- TODO: Hover info of local function signature should be `inner :: Bool`
  innL48 = Position 52  5  ;  innSig = [ExpectHoverText ["inner"], mkR 53 2 53 7]; innSig' = [ExpectHoverText ["inner", "Char"], mkR 49 2 49 7]
  holeL60 = Position 62 7  ;  hleInfo = [ExpectHoverText ["_ ::"]]
  holeL65 = Position 65 8  ;  hleInfo2 = [ExpectHoverText ["_ :: a -> Maybe a"]]
  cccL17 = Position 17 16  ;  docLink = [ExpectHoverTextRegex "\\*Defined in 'GHC.Types'\\* \\*\\(ghc-prim-[0-9.]+\\)\\*\n\n"]
  imported = Position 56 13 ; importedSig = getDocUri "Foo.hs" >>= \foo -> return [ExpectHoverText ["foo", "Foo", "Haddock"], mkL foo 5 0 5 3]
  reexported = Position 55 14 ; reexportedSig = getDocUri "Bar.hs" >>= \bar -> return [ExpectHoverText ["Bar", "Bar", "Haddock"], if ghcVersion >= GHC94 && ghcVersion < GHC910 then mkL bar 3 5 3 8 else mkL bar 3 0 3 14]
  thLocL57 = Position 59 10 ; thLoc = [ExpectHoverText ["Identity"]]
  cmtL68 = Position 67  0  ;  lackOfdEq = [ExpectHoverExcludeText ["$dEq"]]
  import310 = Position 3 10; pkgTxt = [ExpectHoverText ["Data.Text\n\ntext-"]]
  in
  mkFindTests
  --      def    hover  look       expect
  [ -- It suggests either going to the constructor or to the field
    test  (broken fff')  yes               fffL4      fff           "field in record definition"
  , test  yes            yes               fffL8      fff'          "field in record construction    #1102"
  , test  yes            yes               fffL14     fff'          "field name used as accessor"           -- https://github.com/haskell/ghcide/pull/120 in Calculate.hs
  , test  yes            yes               aaaL14     aaa           "top-level name"                        -- https://github.com/haskell/ghcide/pull/120
  , test  yes            yes               dcL7       tcDC          "data constructor record         #1029"
  , test  yes            yes               dcL12      tcDC          "data constructor plain"                -- https://github.com/haskell/ghcide/pull/121
  , test  yes            yes               tcL6       tcData        "type constructor                #1028" -- https://github.com/haskell/ghcide/pull/147
  , test  yes            yes               xtcL5      xtc           "type constructor external   #717,1028"
  , test  yes            yes               xvL20      xvMsg         "value external package           #717" -- https://github.com/haskell/ghcide/pull/120
  , test  yes            yes               vvL16      vv            "plain parameter"                       -- https://github.com/haskell/ghcide/pull/120
  , test  yes            yes               aL18       apmp          "pattern match name"                    -- https://github.com/haskell/ghcide/pull/120
  , test  yes            yes               opL16      op            "top-level operator               #713" -- https://github.com/haskell/ghcide/pull/120
  , test  yes            yes               opL18      opp           "parameter operator"                    -- https://github.com/haskell/ghcide/pull/120
  , test  yes            yes               b'L19      bp            "name in backticks"                     -- https://github.com/haskell/ghcide/pull/120
  , test  yes            yes               clL23      cls           "class in instance declaration   #1027"
  , test  yes            yes               clL25      cls           "class in signature              #1027" -- https://github.com/haskell/ghcide/pull/147
  , test  yes            yes               eclL15     ecls          "external class in signature #717,1027"
  , test  yes            yes               dnbL29     dnb           "do-notation   bind              #1073"
  , test  yes            yes               dnbL30     dnb           "do-notation lookup"
  , test  yes            yes               lcbL33     lcb           "listcomp   bind                 #1073"
  , test  yes            yes               lclL33     lcb           "listcomp lookup"
  , test  yes            yes               mclL36     mcl           "top-level fn 1st clause"
  , test  yes            yes               mclL37     mcl           "top-level fn 2nd clause         #1030"
  , test  yes            yes               spaceL37   space         "top-level fn on space           #1002"
  , test  no             yes               docL41     doc           "documentation                   #1129"
  , test  no             yes               eitL40     kindE         "kind of Either                  #1017"
  , test  no             yes               intL40     kindI         "kind of Int                     #1017"
  , test  no             (broken kindV')   tvrL40     kindV         "kind of (* -> *) type variable  #1017"
  , test  no             (broken litI')    intL41     litI          "literal Int  in hover info      #1016"
  , test  no             (broken litC')    chrL36     litC          "literal Char in hover info      #1016"
  , test  no             (broken litT')    txtL8      litT          "literal Text in hover info      #1016"
  , test  no             (broken litL')    lstL43     litL          "literal List in hover info      #1016"
  , test  yes            yes               cmtL68     lackOfdEq     "no Core symbols                 #3280"
  , test  no             yes               docL41     constr        "type constraint in hover info   #1012"
  , test  no             yes               outL45     outSig        "top-level signature              #767"
  , test  yes            (broken innSig')  innL48     innSig        "inner     signature              #767"
  , test  no             yes               holeL60    hleInfo       "hole without internal name       #831"
  , test  no             yes               holeL65    hleInfo2      "hole with variable"
  , test  no             yes               cccL17     docLink       "Haddock html links"
  , testM yes            yes               imported   importedSig   "Imported symbol"
  , if isWindows then
        -- Flaky on Windows: https://github.com/haskell/haskell-language-server/issues/2997
        testM no     yes    reexported reexportedSig "Imported symbol (reexported)"
    else
        testM yes    yes    reexported reexportedSig "Imported symbol (reexported)"
  , test  no     yes       thLocL57   thLoc         "TH Splice Hover"
  , test yes yes import310 pkgTxt "show package name and its version"
  ]
  where yes :: (TestTree -> Maybe TestTree)
        yes = Just -- test should run and pass
        no = const Nothing -- don't run this test at all
        --skip = const Nothing -- unreliable, don't run
        broken :: [Expect] -> TestTree -> Maybe TestTree
        broken _ = yes

checkFileCompiles :: FilePath -> Session () -> TestTree
checkFileCompiles fp diag =
   testWithDummyPlugin ("hover: Does " ++ fp ++ " compile") (mkIdeTestFs [copyDir "hover"]) $ do
    _ <- openDoc fp "haskell"
    diag

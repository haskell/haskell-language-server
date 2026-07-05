module Main (main) where

import           Control.Lens               ((^.))
import           Data.Char                  (isSpace)
import           Data.Either                (rights)
import           Data.List                  (sort)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           Ide.Plugin.Export          (descriptor)
import qualified Language.LSP.Protocol.Lens as L
import           System.FilePath            ((</>))
import           Test.Hls
import           Test.Hls.FileSystem        (copy, directProject,
                                             mkVirtualFileTree)

plugin :: PluginTestDescriptor ()
plugin = mkPluginTestDescriptor' descriptor "export"

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-export-plugin" </> "test" </> "testdata"

-- | Open the named module in its own temporary single-file project, so each
-- test compiles only the file it needs and cannot pick up signals from a
-- sibling module.
runExport :: FilePath -> (TextDocumentIdentifier -> Session a) -> IO a
runExport = runExportWith []

-- | Like 'runExport' but also copies the named extra files into the project,
-- e.g. a header a CPP @#include@ pulls in next to the module.
runExportWith :: [FilePath] -> FilePath -> (TextDocumentIdentifier -> Session a) -> IO a
runExportWith extra hsFile act =
    runSessionWithTestConfig def
        { testDirLocation = Right (mkVirtualFileTree testDataDir (directProject hsFile <> map copy extra))
        , testPluginDescriptor = plugin
        } $ \_dir -> do
            doc <- openDoc hsFile "haskell"
            waitForKickDone
            act doc

executeExportAction :: TextDocumentIdentifier -> Range -> Session ()
executeExportAction doc range = do
    actions <- rights . map toEither <$> getCodeActions doc range
    case filter (\ca -> "Export `" `T.isPrefixOf` (ca ^. L.title)) actions of
        (ca:_) -> executeCodeAction ca
        []     -> liftIO $ assertFailure "Export `...` action not offered"

noExportOffered :: TextDocumentIdentifier -> Range -> Session ()
noExportOffered doc range = do
    titles <- sort . map (^. L.title) . rights . map toEither <$> getCodeActions doc range
    liftIO $ not (any ("Export `" `T.isPrefixOf`) titles)
        @? ("Did not expect an Export action; saw: " <> show titles)

-- | Fail unless some variant is an infix of the text. The message dumps it.
assertAnyInfix :: T.Text -> [T.Text] -> Assertion
assertAnyInfix hay variants =
    any (`T.isInfixOf` hay) variants
        @? ("Expected one of " <> show variants <> " in:\n" <> T.unpack hay)

containsAfter :: TextDocumentIdentifier -> [T.Text] -> Session ()
containsAfter doc expected = documentContents doc >>= liftIO . (`assertAnyInfix` expected)

-- | Fail unless every needle is an infix of the haystack. Used to assert that
-- CPP directives and conditional items survive an edit verbatim.
assertContainsAll :: T.Text -> [T.Text] -> Assertion
assertContainsAll hay = mapM_ $ \needle ->
    needle `T.isInfixOf` hay
        @? ("Expected " <> show needle <> " in:\n" <> T.unpack hay)

-- | Lines from the first @(@ through the @) where@ line (included, so an item on
-- the closing line still counts).
exportListRegion :: T.Text -> [T.Text]
exportListRegion txt =
    let afterOpen     = dropWhile (not . T.isInfixOf "(") (T.lines txt)
        (body, close) = break (T.isInfixOf ") where") afterOpen
    in body ++ take 1 close

-- | Does @name@ appear in the export list at CPP nesting level 0, i.e. not
-- guarded by any @#if@/@#ifdef@/@#ifndef@?
exportedUnconditionally :: T.Text -> T.Text -> Bool
exportedUnconditionally name txt = go (0 :: Int) (exportListRegion txt)
  where
    go _ [] = False
    go n (l:ls)
        | "#if"    `T.isPrefixOf` T.stripStart l = go (n + 1) ls
        | "#endif" `T.isPrefixOf` T.stripStart l = go (max 0 (n - 1)) ls
        | n == 0, name `T.isInfixOf` l           = True
        | otherwise                              = go n ls

-- | True when the export-list region carries no doubled or leading comma. A
-- trailing comma before @)@ is legal Haskell, so @,)@ is not flagged.
wellFormedExportList :: T.Text -> Bool
wellFormedExportList txt = not (any (`T.isInfixOf` compact) ["(,", ",,"])
  where
    compact = T.filter (not . isSpace) (T.unlines (exportListRegion txt))

-- | Run the export action, assert the result is well-formed, return its text.
exportAndCheck :: TextDocumentIdentifier -> Range -> Session T.Text
exportAndCheck doc pos = do
    executeExportAction doc pos
    txt <- documentContents doc
    liftIO $ wellFormedExportList txt
        @? ("malformed export list, got:\n" <> T.unpack txt)
    pure txt

-- | Crudely re-run CPP for the macro EXAMPLE_FLAG over already-edited text,
-- keeping the branch the given definedness selects. Single level, just enough
-- to inspect the configuration the server did not parse.
preprocessExampleFlag :: Bool -> T.Text -> T.Text
preprocessExampleFlag defined = T.unlines . go Nothing . T.lines
  where
    -- Nothing outside any conditional. Just b inside one, emitting only when b.
    go _ [] = []
    go st (l:ls)
        | isDir "#ifdef"  = go (Just defined) ls
        | isDir "#ifndef" = go (Just (not defined)) ls
        | isDir "#else"   = go (fmap not st) ls
        | isDir "#endif"  = go Nothing ls
        | fromMaybe True st = l : go st ls
        | otherwise         = go st ls
      where isDir d = d `T.isPrefixOf` T.stripStart l

rangeAt :: UInt -> UInt -> Range
rangeAt l c = Range (Position l c) (Position l c)

-- | The CPP block the testdata guards with @#ifdef EXAMPLE_FLAG@. The flag is
-- never defined, so the branch is inactive and must survive an edit verbatim.
flagBlock :: [T.Text]
flagBlock = ["#ifdef EXAMPLE_FLAG", ", flagged", "#endif"]

-- | The new export of @name@ must sit at CPP nesting level 0, never in a branch.
assertExportedUnconditionally :: T.Text -> T.Text -> Assertion
assertExportedUnconditionally name txt =
    exportedUnconditionally name txt
        @? (T.unpack name <> " must be exported outside any CPP branch, got:\n" <> T.unpack txt)

-- | The 'flagBlock' survives verbatim and @name@ lands outside it.
assertFlaggedBlockKept :: T.Text -> T.Text -> Assertion
assertFlaggedBlockKept name txt =
    assertContainsAll txt flagBlock >> assertExportedUnconditionally name txt

-- | Export the binding at the position and assert the result contains one of
-- the @expected@ variants.
addCase :: TestName -> FilePath -> UInt -> UInt -> [T.Text] -> TestTree
addCase name file l c expected = testCase name $ runExport file $ \doc -> do
    executeExportAction doc (rangeAt l c)
    containsAfter doc expected

-- | Assert no export action is offered at the position.
noCase :: TestName -> FilePath -> UInt -> UInt -> TestTree
noCase name file l c = testCase name $ runExport file $ \doc ->
    noExportOffered doc (rangeAt l c)

-- | Export the binding at the position, assert the list is well-formed, then
-- run @check@ over the resulting document text.
exportCase :: TestName -> FilePath -> UInt -> UInt -> (T.Text -> Assertion) -> TestTree
exportCase name file l c check = testCase name $ runExport file $ \doc -> do
    txt <- exportAndCheck doc (rangeAt l c)
    liftIO (check txt)

main :: IO ()
main = defaultTestRunner $ testGroup "Export"
    [ testGroup "Add: value bindings"
        [ addCase "add value to export list" "AddExport.hs" 6 0
            ["module AddExport (foo, Bar, bar)"]
        , noCase "no action when value already exported" "AddExport.hs" 3 0  -- on `foo`
        , addCase "append follows a multi-line leading-comma list" "AddExportMultiline.hs" 11 0  -- on `baz`
            ["  , baz\n  ) where"]
        ]

    , testGroup "Add: type declarations"
        [ addCase "add bare type as T(..)" "AddExport.hs" 9 5  -- on `Baz` type name
            ["Baz(..)", "Baz (..)"]
        ]

    , testGroup "Add: constructors"
        [ addCase "constructor with no parent entry appends T (C)" "AddExport.hs" 9 12  -- on `Baz1`, no Baz entry yet
            ["Baz (Baz1)", "Baz(Baz1)"]
        , addCase "constructor under bare-type parent promotes to T(C)" "AddCtor.hs" 3 11  -- on `Bar1`, Bar is IEThingAbs
            ["Bar (Bar1)", "Bar(Bar1)"]
        , addCase "constructor merges into existing IEThingWith parent" "AddCtor.hs" 2 18  -- on `Foo2`, Foo has [Foo1]
            ["Foo (Foo1, Foo2)", "Foo(Foo1, Foo2)"]
        , noCase "constructor already in IEThingWith children suppresses action" "AddCtor.hs" 2 11  -- on `Foo1`, already child of Foo(Foo1)
        , noCase "constructor under IEThingAll T(..) suppresses action" "AddCtor.hs" 4 11  -- on `Baz1`, Baz(..) covers it
        , noCase "constructor exported standalone suppresses action" "AddCtor.hs" 5 11  -- on `Qux1`, Qux1 standalone in list
        ]

    , testGroup "Add: type classes"
        [ addCase "add class as T(..)" "AddClass.hs" 8 6  -- on `Baz` class name
            ["module AddClass (Foo (..), Bar, Baz (..))"]
        , noCase "no add action when class exported as T(..)" "AddClass.hs" 2 6  -- on `Foo`, exported as Foo (..)
        , noCase "no add action when class exported as bare T" "AddClass.hs" 5 6  -- on `Bar`, exported as bare
        , noCase "no add action on class method" "AddClass.hs" 9 2  -- on `baz1` inside `class Baz a where`
        ]

    , testGroup "Add: layout variants"
        [ addCase "add to an empty export list" "AddExportEmpty.hs" 2 0  -- on `foo`
            ["module AddExportEmpty (foo) where"]
        , addCase "append after a trailing comma" "AddExportTrailingComma.hs" 7 0  -- on `bar`
            ["( foo, bar"]
        , addCase "preserve a haddock comment between items" "AddExportComment.hs" 16 0  -- on `quux`
            ["  -- * For testing\n  , baz\n  , quux\n  ) where"]
        ]

    , testGroup "Add: declaration kinds"
        [ addCase "function operator is parenthesized" "AddExportKinds.hs" 8 1  -- on `(<|)`
            ["(placeholder, (<|))"]
        , addCase "infix function exports bare name" "AddExportKinds.hs" 11 3  -- on `f`
            ["(placeholder, f)"]
        , addCase "newtype exports as T(..)" "AddExportKinds.hs" 13 8  -- on `NT`
            ["placeholder, NT(..)", "placeholder, NT (..)"]
        , addCase "type synonym exports bare" "AddExportKinds.hs" 15 5  -- on `Syn`
            ["(placeholder, Syn)"]
        , addCase "type family exports bare" "AddExportKinds.hs" 17 12  -- on `TF`
            ["(placeholder, TF)"]
        , addCase "pattern synonym gets a pattern prefix" "AddExportKinds.hs" 20 9  -- on `Pat`
            ["(placeholder, pattern Pat)"]
        , addCase "data operator gets type keyword and (..)" "AddExportKinds.hs" 22 7  -- on `(:<)`
            ["placeholder, type (:<)(..)", "placeholder, type (:<) (..)"]
        ]

    , testGroup "Add: type-level operators"
        [ addCase "type synonym operator has no type keyword" "AddExportTypeOps.hs" 8 7  -- on `(:<>)`
            ["(placeholder, (:<>))"]
        , addCase "type family operator gets type keyword" "AddExportTypeOps.hs" 10 14  -- on `(:+:)`
            ["(placeholder, type (:+:))"]
        , addCase "typeclass operator gets type keyword and (..)" "AddExportTypeOps.hs" 12 8  -- on `(:*:)`
            ["placeholder, type (:*:)(..)", "placeholder, type (:*:) (..)"]
        , addCase "newtype operator gets type keyword and (..)" "AddExportTypeOps.hs" 14 10  -- on `(:->)`
            ["placeholder, type (:->)(..)", "placeholder, type (:->) (..)"]
        , addCase "pattern synonym operator is parenthesized" "AddExportTypeOps.hs" 16 11  -- on `(:++)`
            ["(placeholder, pattern (:++))"]
        ]

    , testGroup "Add: negative cases"
        [ noCase "no action on implicit module" "Implicit.hs" 3 0
        , noCase "no action when cursor on RHS" "AddExport.hs" 6 6  -- col 6 is on the `2` of `bar = 2`
        , noCase "no action on a where-bound name" "AddExportNegatives.hs" 7 8  -- on `whereBound`
        , noCase "no action on a record field" "AddExportNegatives.hs" 9 18  -- on `recField`
        ]

    , testGroup "Add: CPP in the export list"
        -- EXAMPLE_FLAG is never defined in the test project, so #ifdef branches
        -- are inactive and #ifndef branches are active. The edit must preserve
        -- every directive verbatim and place the new export outside any branch.
        [ exportCase "preserves a trailing #ifdef block" "CppExportTail.hs" 15 0  -- on `baz`
            (assertFlaggedBlockKept "baz")

        , exportCase "preserves a leading #ifndef block" "CppExportHead.hs" 12 0 $ \txt -> do  -- on `bar`
            -- the whole guarded block survives verbatim, not just stray substrings
            assertContainsAll txt ["#ifndef EXAMPLE_FLAG\n    foo\n#endif"]
            assertExportedUnconditionally "bar" txt

        , exportCase "preserves both #if/#else branches" "CppExportElse.hs" 20 0 $ \txt -> do  -- on `extra`
            assertContainsAll txt
                ["#ifdef EXAMPLE_FLAG", ", windows", "#else", ", posix", "#endif"]
            assertExportedUnconditionally "extra" txt

        , testCase "preserves an #include directive" $ runExportWith ["CppExportInclude.h"] "CppExportInclude.hs" $ \doc -> do
            txt <- exportAndCheck doc (rangeAt 13 0)  -- on `extra`
            liftIO $ do
                assertContainsAll txt ["#include \"CppExportInclude.h\"", "( extra, foo"]
                assertExportedUnconditionally "extra" txt

        , exportCase "appends a new T(C) beside a CPP block" "CppCtorAppend.hs" 11 11 $ \txt -> do  -- on `Baz1`, no Baz entry yet
            assertFlaggedBlockKept "Baz1" txt
            txt `assertAnyInfix` ["Baz (Baz1)", "Baz(Baz1)"]

        , exportCase "adds a separate entry beside an IEThingWith parent" "CppCtorExtend.hs" 8 18 $ \txt -> do  -- on `Foo2`, Foo has [Foo1]
            assertFlaggedBlockKept "Foo2" txt
            assertContainsAll txt ["Foo(Foo1)"]
            txt `assertAnyInfix` ["Foo (Foo2)", "Foo(Foo2)"]

        , exportCase "adds a separate entry without a double comma" "CppCtorMid.hs" 9 18 $ \txt -> do  -- on `Foo2`, Foo(Foo1) precedes `, bar`
            assertContainsAll txt (flagBlock <> [", bar", "Foo(Foo1)"])
            txt `assertAnyInfix` ["Foo (Foo2)", "Foo(Foo2)"]

        , exportCase "adds a separate entry beside a bare-type parent" "CppCtorUpgrade.hs" 8 11 $ \txt -> do  -- on `Bar1`, Bar is IEThingAbs
            assertFlaggedBlockKept "Bar1" txt
            txt `assertAnyInfix` ["Bar (Bar1)", "Bar(Bar1)"]

        , exportCase "exports an operator beside a CPP block" "CppExportKinds.hs" 12 1  -- on `(<|)`
            (assertFlaggedBlockKept "(<|)")

        , exportCase "exports a pattern synonym beside a CPP block" "CppExportKinds.hs" 16 8  -- on `Zero`
            (assertFlaggedBlockKept "pattern Zero")

        , exportCase "adds a separate entry beside a directive inside a constructor list" "CppCtorIntra.hs" 9 24 $ \txt -> do  -- on `Foo2`
            -- the #ifdef sits inside Foo(...), where an in-place merge would erase it
            assertContainsAll txt ["#ifdef EXAMPLE_FLAG\n      , Bar\n#endif", "Foo(Foo1"]
            txt `assertAnyInfix` ["Foo (Foo2)", "Foo(Foo2)"]

        , exportCase "front-inserts even when the close paren shares a line" "CppExportParenShared.hs" 18 0 $ \txt -> do  -- on `baz`
            assertContainsAll txt (flagBlock <> [", bar )"])
            assertExportedUnconditionally "baz" txt

        , exportCase "no double comma when the last item already has a trailing comma" "CppExportTrailingComma.hs" 15 0 $ \txt -> do  -- on `baz`
            -- a doubled `,,` would be caught by exportAndCheck's well-formedness check
            assertContainsAll txt ["#ifdef EXAMPLE_FLAG", "flagged,", "#endif"]
            assertExportedUnconditionally "baz" txt

        , exportCase "edit stays valid in the unparsed CPP branch" "CppExportOtherBranch.hs" 12 0 $ \txt -> do  -- on `bar`, the only item is in the other branch
            -- the single item lives under #ifndef, so it is the whole parsed list.
            -- The front-insert plus trailing comma stays valid when the flag flips
            -- and that item disappears.
            let otherBranch = preprocessExampleFlag True txt
            wellFormedExportList otherBranch
                @? ("edit breaks the EXAMPLE_FLAG-defined configuration:\n" <> T.unpack otherBranch)
        ]

    , testGroup "Export fixes the unused-binding warning"
        [ knownBrokenForGhcVersions [GHC96]
            "TcRnUnusedName provenance is unstructured before GHC 9.8 (GHC #20115)" $
          testCase "Export action attaches the -Wunused-top-binds diagnostic" $ runExport "ExportUnusedFix.hs" $ \doc -> do
            actions <- rights . map toEither <$> getCodeActions doc (rangeAt 6 0)  -- on `unused`
            case filter ((== "Export `unused`") . (^. L.title)) actions of
                (ca:_) -> liftIO $ not (null (fromMaybe [] (ca ^. L.diagnostics)))
                            @? "Export action should carry the unused-binding diagnostic"
                []     -> liftIO $ assertFailure $
                            "Export `unused` not offered; saw: " <> show (map (^. L.title) actions)
        ]
    ]

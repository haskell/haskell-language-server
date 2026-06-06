module Main (main) where

import           Control.Lens               ((^.))
import           Data.Either                (rights)
import           Data.List                  (sort)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           Ide.Plugin.Export          (descriptor)
import qualified Language.LSP.Protocol.Lens as L
import           System.FilePath            ((</>))
import           Test.Hls

plugin :: PluginTestDescriptor ()
plugin = mkPluginTestDescriptor' descriptor "export"

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-export-plugin" </> "test" </> "testdata"

runExport :: (FilePath -> Session a) -> IO a
runExport act =
    runSessionWithTestConfig def
        { testDirLocation = Left testDataDir
        , testPluginDescriptor = plugin
        } act

codeActionTitles :: TextDocumentIdentifier -> Range -> Session [T.Text]
codeActionTitles doc range =
    sort . map (^. L.title) . rights . map toEither
        <$> getCodeActions doc range

executeByPrefix :: T.Text -> TextDocumentIdentifier -> Range -> Session ()
executeByPrefix prefix doc range = do
    actions <- rights . map toEither <$> getCodeActions doc range
    case filter (\ca -> prefix `T.isPrefixOf` (ca ^. L.title)) actions of
        (ca:_) -> executeCodeAction ca
        []     -> liftIO $ assertFailure (T.unpack prefix <> "...` action not offered")

executeExportAction :: TextDocumentIdentifier -> Range -> Session ()
executeExportAction = executeByPrefix "Export `"

noActionWithPrefix :: T.Text -> TextDocumentIdentifier -> Range -> Session ()
noActionWithPrefix prefix doc range = do
    titles <- codeActionTitles doc range
    liftIO $ not (any (prefix `T.isPrefixOf`) titles)
        @? ("Did not expect " <> T.unpack prefix <> " action; saw: " <> show titles)

noExportOffered :: TextDocumentIdentifier -> Range -> Session ()
noExportOffered = noActionWithPrefix "Export `"

-- | Fail unless some variant is an infix of the text. The message dumps it.
assertAnyInfix :: T.Text -> [T.Text] -> Assertion
assertAnyInfix hay variants =
    any (`T.isInfixOf` hay) variants
        @? ("Expected one of " <> show variants <> " in:\n" <> T.unpack hay)

containsAfter :: TextDocumentIdentifier -> [T.Text] -> Session ()
containsAfter doc expected = documentContents doc >>= liftIO . (`assertAnyInfix` expected)

rangeAt :: UInt -> UInt -> Range
rangeAt l c = Range (Position l c) (Position l c)

main :: IO ()
main = defaultTestRunner $ testGroup "Export"
    [ testGroup "Add: value bindings"
        [ testCase "add value to export list" $ runExport $ \_dir -> do
            doc <- openDoc "AddExport.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 6 0)
            containsAfter doc ["module AddExport (foo, Bar, bar)"]

        , testCase "no action when value already exported" $ runExport $ \_dir -> do
            doc <- openDoc "AddExport.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 3 0)  -- on `foo`

        , testCase "append follows a multi-line leading-comma list" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportMultiline.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 11 0)  -- on `baz`
            containsAfter doc ["  , baz\n  ) where"]
        ]

    , testGroup "Add: type declarations"
        [ testCase "add bare type as T(..)" $ runExport $ \_dir -> do
            doc <- openDoc "AddExport.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 9 5)  -- on `Baz` type name
            containsAfter doc ["Baz(..)", "Baz (..)"]
        ]

    , testGroup "Add: constructors"
        [ testCase "constructor with no parent entry appends T (C)" $ runExport $ \_dir -> do
            doc <- openDoc "AddExport.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 9 12)  -- on `Baz1`, no Baz entry yet
            containsAfter doc ["Baz (Baz1)", "Baz(Baz1)"]

        , testCase "constructor under bare-type parent promotes to T(C)" $ runExport $ \_dir -> do
            doc <- openDoc "AddCtor.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 3 11)  -- on `Bar1`, Bar is IEThingAbs
            containsAfter doc ["Bar (Bar1)", "Bar(Bar1)"]

        , testCase "constructor merges into existing IEThingWith parent" $ runExport $ \_dir -> do
            doc <- openDoc "AddCtor.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 2 18)  -- on `Foo2`, Foo has [Foo1]
            containsAfter doc ["Foo (Foo1, Foo2)", "Foo(Foo1, Foo2)"]

        , testCase "constructor already in IEThingWith children suppresses action" $ runExport $ \_dir -> do
            doc <- openDoc "AddCtor.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 2 11)  -- on `Foo1`, already child of Foo(Foo1)

        , testCase "constructor under IEThingAll T(..) suppresses action" $ runExport $ \_dir -> do
            doc <- openDoc "AddCtor.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 4 11)  -- on `Baz1`, Baz(..) covers it

        , testCase "constructor exported standalone suppresses action" $ runExport $ \_dir -> do
            doc <- openDoc "AddCtor.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 5 11)  -- on `Qux1`, Qux1 standalone in list
        ]

    , testGroup "Add: type classes"
        [ testCase "add class as T(..)" $ runExport $ \_dir -> do
            doc <- openDoc "AddClass.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 8 6)  -- on `Baz` class name
            containsAfter doc ["module AddClass (Foo (..), Bar, Baz (..))"]

        , testCase "no add action when class exported as T(..)" $ runExport $ \_dir -> do
            doc <- openDoc "AddClass.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 2 6)  -- on `Foo`, exported as Foo (..)

        , testCase "no add action when class exported as bare T" $ runExport $ \_dir -> do
            doc <- openDoc "AddClass.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 5 6)  -- on `Bar`, exported as bare

        , testCase "no add action on class method" $ runExport $ \_dir -> do
            doc <- openDoc "AddClass.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 9 2)  -- on `baz1` inside `class Baz a where`
        ]

    , testGroup "Add: layout variants"
        [ testCase "add to an empty export list" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportEmpty.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 2 0)  -- on `foo`
            containsAfter doc ["module AddExportEmpty (foo) where"]

        , testCase "append after a trailing comma" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportTrailingComma.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 7 0)  -- on `bar`
            containsAfter doc ["( foo, bar"]

        , testCase "preserve a haddock comment between items" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportComment.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 16 0)  -- on `quux`
            containsAfter doc ["  -- * For testing\n  , baz\n  , quux\n  ) where"]
        ]

    , testGroup "Add: declaration kinds"
        [ testCase "function operator is parenthesized" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportKinds.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 8 1)  -- on `(<|)`
            containsAfter doc ["(placeholder, (<|))"]

        , testCase "infix function exports bare name" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportKinds.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 11 3)  -- on `f`
            containsAfter doc ["(placeholder, f)"]

        , testCase "newtype exports as T(..)" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportKinds.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 13 8)  -- on `NT`
            containsAfter doc ["placeholder, NT(..)", "placeholder, NT (..)"]

        , testCase "type synonym exports bare" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportKinds.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 15 5)  -- on `Syn`
            containsAfter doc ["(placeholder, Syn)"]

        , testCase "type family exports bare" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportKinds.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 17 12)  -- on `TF`
            containsAfter doc ["(placeholder, TF)"]

        , testCase "pattern synonym gets a pattern prefix" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportKinds.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 20 9)  -- on `Pat`
            containsAfter doc ["(placeholder, pattern Pat)"]

        , testCase "data operator gets type keyword and (..)" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportKinds.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 22 7)  -- on `(:<)`
            containsAfter doc ["placeholder, type (:<)(..)", "placeholder, type (:<) (..)"]
        ]

    , testGroup "Add: type-level operators"
        [ testCase "type synonym operator has no type keyword" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportTypeOps.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 8 7)  -- on `(:<>)`
            containsAfter doc ["(placeholder, (:<>))"]

        , testCase "type family operator gets type keyword" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportTypeOps.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 10 14)  -- on `(:+:)`
            containsAfter doc ["(placeholder, type (:+:))"]

        , testCase "typeclass operator gets type keyword and (..)" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportTypeOps.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 12 8)  -- on `(:*:)`
            containsAfter doc ["placeholder, type (:*:)(..)", "placeholder, type (:*:) (..)"]

        , testCase "newtype operator gets type keyword and (..)" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportTypeOps.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 14 10)  -- on `(:->)`
            containsAfter doc ["placeholder, type (:->)(..)", "placeholder, type (:->) (..)"]

        , testCase "pattern synonym operator is parenthesized" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportTypeOps.hs" "haskell"
            waitForKickDone
            executeExportAction doc (rangeAt 16 11)  -- on `(:++)`
            containsAfter doc ["(placeholder, pattern (:++))"]
        ]

    , testGroup "Add: negative cases"
        [ testCase "no action on implicit module" $ runExport $ \_dir -> do
            doc <- openDoc "Implicit.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 3 0)

        , testCase "no action when cursor on RHS" $ runExport $ \_dir -> do
            doc <- openDoc "AddExport.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 6 6)  -- col 6 is on the `2` of `bar = 2`

        , testCase "no action on a where-bound name" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportNegatives.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 7 8)  -- on `whereBound`

        , testCase "no action on a record field" $ runExport $ \_dir -> do
            doc <- openDoc "AddExportNegatives.hs" "haskell"
            waitForKickDone
            noExportOffered doc (rangeAt 9 18)  -- on `recField`
        ]

    , testGroup "Export fixes the unused-binding warning"
        [ testCase "Export action attaches the -Wunused-top-binds diagnostic" $ runExport $ \_dir -> do
            doc <- openDoc "ExportUnusedFix.hs" "haskell"
            waitForKickDone
            actions <- rights . map toEither <$> getCodeActions doc (rangeAt 6 0)  -- on `unused`
            case filter ((== "Export `unused`") . (^. L.title)) actions of
                (ca:_) -> liftIO $ not (null (fromMaybe [] (ca ^. L.diagnostics)))
                            @? "Export action should carry the unused-binding diagnostic"
                []     -> liftIO $ assertFailure $
                            "Export `unused` not offered; saw: " <> show (map (^. L.title) actions)
        ]
    ]

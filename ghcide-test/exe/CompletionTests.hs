
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module CompletionTests (tests) where

import           Config
import           Control.Lens                   ((^.))
import qualified Control.Lens                   as Lens
import           Control.Monad
import           Control.Monad.IO.Class         (liftIO)
import           Data.Default
import           Data.List.Extra
import           Data.Maybe
import qualified Data.Text                      as T
import           Development.IDE.Types.Location
import           Ide.Plugin.Config
import qualified Language.LSP.Protocol.Lens     as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types    hiding
                                                (SemanticTokenAbsolute (..),
                                                 SemanticTokenRelative (..),
                                                 SemanticTokensEdit (..),
                                                 mkRange)
import           Language.LSP.Test
import           Test.Hls                       (waitForTypecheck)
import qualified Test.Hls.FileSystem            as FS
import           Test.Hls.FileSystem            (file, text)
import           Test.Hls.Util
import           Test.Tasty
import           Test.Tasty.HUnit


tests :: TestTree
tests
  = testGroup "completion"
    [
    testGroup "non local" nonLocalCompletionTests
    , testGroup "topLevel" topLevelCompletionTests
    , testGroup "local" localCompletionTests
    , testGroup "package" packageCompletionTests
    , testGroup "project" projectCompletionTests
    , testGroup "other" otherCompletionTests
    , testGroup "doc" completionDocTests
    ]

testSessionEmpty :: TestName -> Session () -> TestTree
testSessionEmpty name = testWithDummyPlugin name (mkIdeTestFs [FS.directCradle ["A.hs"]])

testSessionEmptyWithCradle :: TestName -> T.Text -> Session () -> TestTree
testSessionEmptyWithCradle name cradle = testWithDummyPlugin name (mkIdeTestFs [file "hie.yaml" (text cradle)])

testSessionSingleFile :: TestName -> FilePath -> T.Text -> Session () -> TestTree
testSessionSingleFile testName fp txt session =
    testWithDummyPlugin testName (mkIdeTestFs [FS.directCradle [T.pack fp] , file fp (text txt)]) session

completionTest :: HasCallStack => String -> [T.Text] -> Position -> [(T.Text, CompletionItemKind, T.Text, Bool, Bool, Maybe [TextEdit])] -> TestTree
completionTest name src pos expected = testSessionSingleFile name "A.hs" (T.unlines src) $ do
    docId <- openDoc "A.hs" "haskell"
    _ <- waitForDiagnostics
    compls <- getAndResolveCompletions docId pos
    let compls' = [ (_label, _kind, _insertText, _additionalTextEdits) | CompletionItem{..} <- compls]
    let emptyToMaybe x = if T.null x then Nothing else Just x
    liftIO $ sortOn (Lens.view Lens._1) (take (length expected) compls') @?=
        sortOn (Lens.view Lens._1)
          [ (l, Just k, emptyToMaybe t, at) | (l,k,t,_,_,at) <- expected]
    forM_ (zip compls expected) $ \(CompletionItem{..}, (_,_,_,expectedSig, expectedDocs, _)) -> do
        when expectedSig $
            liftIO $ assertBool ("Missing type signature: " <> T.unpack _label) (isJust _detail)
        when expectedDocs $
            liftIO $ assertBool ("Missing docs: " <> T.unpack _label) (isJust _documentation)


topLevelCompletionTests :: [TestTree]
topLevelCompletionTests = [
    completionTest
        "variable"
        ["bar = xx", "-- | haddock", "xxx :: ()", "xxx = ()", "-- | haddock", "data Xxx = XxxCon"]
        (Position 0 8)
        [("xxx", CompletionItemKind_Function, "xxx", True, True, Nothing)
        ],
    completionTest
        "constructor"
        ["bar = xx", "-- | haddock", "xxx :: ()", "xxx = ()", "-- | haddock", "data Xxx = XxxCon"]
        (Position 0 8)
        [("xxx", CompletionItemKind_Function, "xxx", True, True, Nothing)
        ],
    completionTest
        "class method"
        ["bar = xx", "class Xxx a where", "-- | haddock", "xxx :: ()", "xxx = ()"]
        (Position 0 8)
        [("xxx", CompletionItemKind_Function, "xxx", True, True, Nothing)],
    completionTest
        "type"
        ["bar :: Xz", "zzz = ()", "-- | haddock", "data Xzz = XzzCon"]
        (Position 0 9)
        [("Xzz", CompletionItemKind_Struct, "Xzz", False, True, Nothing)],
    completionTest
        "class"
        ["bar :: Xz", "zzz = ()", "-- | haddock", "class Xzz a"]
        (Position 0 9)
        [("Xzz", CompletionItemKind_Interface, "Xzz", False, True, Nothing)],
    completionTest
        "records"
        ["data Person = Person { _personName:: String, _personAge:: Int}", "bar = Person { _pers }" ]
        (Position 1 19)
        [("_personName", CompletionItemKind_Function, "_personName", False, True, Nothing),
         ("_personAge", CompletionItemKind_Function, "_personAge", False, True, Nothing)],
    completionTest
        "recordsConstructor"
        ["data XxRecord = XyRecord { x:: String, y:: Int}", "bar = Xy" ]
        (Position 1 19)
        [("XyRecord", CompletionItemKind_Constructor, "XyRecord", False, True, Nothing),
         ("XyRecord", CompletionItemKind_Snippet, "XyRecord {x=${1:_x}, y=${2:_y}}", False, True, Nothing)]
    ]

localCompletionTests :: [TestTree]
localCompletionTests = [
    completionTest
        "argument"
        ["bar (Just abcdef) abcdefg = abcd"]
        (Position 0 32)
        [("abcdef", CompletionItemKind_Function, "abcdef", True, False, Nothing),
         ("abcdefg", CompletionItemKind_Function , "abcdefg", True, False, Nothing)
        ],
    completionTest
        "let"
        ["bar = let (Just abcdef) = undefined"
        ,"          abcdefg = let abcd = undefined in undefined"
        ,"        in abcd"
        ]
        (Position 2 15)
        [("abcdef", CompletionItemKind_Function, "abcdef", True, False, Nothing),
         ("abcdefg", CompletionItemKind_Function , "abcdefg", True, False, Nothing)
        ],
    completionTest
        "where"
        ["bar = abcd"
        ,"  where (Just abcdef) = undefined"
        ,"        abcdefg = let abcd = undefined in undefined"
        ]
        (Position 0 10)
        [("abcdef", CompletionItemKind_Function, "abcdef", True, False, Nothing),
         ("abcdefg", CompletionItemKind_Function , "abcdefg", True, False, Nothing)
        ],
    completionTest
        "do/1"
        ["bar = do"
        ,"  Just abcdef <- undefined"
        ,"  abcd"
        ,"  abcdefg <- undefined"
        ,"  pure ()"
        ]
        (Position 2 6)
        [("abcdef", CompletionItemKind_Function, "abcdef", True, False, Nothing)
        ],
    completionTest
        "do/2"
        ["bar abcde = do"
        ,"    Just [(abcdef,_)] <- undefined"
        ,"    abcdefg <- undefined"
        ,"    let abcdefgh = undefined"
        ,"        (Just [abcdefghi]) = undefined"
        ,"    abcd"
        ,"  where"
        ,"    abcdefghij = undefined"
        ]
        (Position 5 8)
        [("abcde", CompletionItemKind_Function, "abcde", True, False, Nothing)
        ,("abcdefghij", CompletionItemKind_Function, "abcdefghij", True, False, Nothing)
        ,("abcdef", CompletionItemKind_Function, "abcdef", True, False, Nothing)
        ,("abcdefg", CompletionItemKind_Function, "abcdefg", True, False, Nothing)
        ,("abcdefgh", CompletionItemKind_Function, "abcdefgh", True, False, Nothing)
        ,("abcdefghi", CompletionItemKind_Function, "abcdefghi", True, False, Nothing)
        ],
    completionTest
        "type family"
        ["{-# LANGUAGE DataKinds, TypeFamilies #-}"
        ,"type family Bar a"
        ,"a :: Ba"
        ]
        (Position 2 7)
        [("Bar", CompletionItemKind_Struct, "Bar", True, False, Nothing)
        ],
    completionTest
        "class method"
        [
          "class Test a where"
        , "    abcd :: a -> ()"
        , "    abcde :: a -> Int"
        , "instance Test Int where"
        , "    abcd = abc"
        ]
        (Position 4 14)
        [("abcd", CompletionItemKind_Function, "abcd", True, False, Nothing)
        ,("abcde", CompletionItemKind_Function, "abcde", True, False, Nothing)
        ],
    testSessionEmpty "incomplete entries" $ do
        let src a = "data Data = " <> a
        doc <- createDoc "A.hs" "haskell" $ src "AAA"
        void $ waitForTypecheck doc
        let editA rhs =
                changeDoc doc [TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $ src rhs]
        editA "AAAA"
        void $ waitForTypecheck doc
        editA "AAAAA"
        void $ waitForTypecheck doc

        compls <- getCompletions doc (Position 0 15)
        liftIO $ filter ("AAA" `T.isPrefixOf`) (mapMaybe _insertText compls) @?= ["AAAAA"]
        pure ()
    ]

nonLocalCompletionTests :: [TestTree]
nonLocalCompletionTests =
  [ brokenForWinOldGhc $ completionTest
      "variable"
      ["module A where", "f = hea"]
      (Position 1 7)
      [("head", CompletionItemKind_Function, "head", True, True, Nothing)],
    completionTest
      "constructor"
      ["{-# OPTIONS_GHC -Wall #-}", "module A where", "f = True"]
      (Position 2 8)
      [ ("True", CompletionItemKind_Constructor, "True", True, True, Nothing)
      ],
    brokenForWinGhc $ completionTest
      "type"
      ["{-# OPTIONS_GHC -Wall #-}", "module A () where", "f :: Boo", "f = True"]
      (Position 2 8)
      [ ("Bool", CompletionItemKind_Struct, "Bool", True, True, Nothing)
      ],
    completionTest
      "qualified"
      ["{-# OPTIONS_GHC -Wunused-binds #-}", "module A () where", "f = Prelude.hea"]
      (Position 2 15)
      [ ("head", CompletionItemKind_Function, "head", True, True, Nothing)
      ],
    completionTest
      "duplicate import"
      ["module A where", "import Data.List", "import Data.List", "f = permu"]
      (Position 3 9)
      [ ("permutations", CompletionItemKind_Function, "permutations", False, False, Nothing)
      ],
    completionTest
       "dont show hidden items"
       [ "{-# LANGUAGE NoImplicitPrelude #-}",
         "module A where",
         "import Control.Monad hiding (join)",
         "f = joi"
       ]
       (Position 3 6)
       [],
    testGroup "ordering"
      [completionTest "qualified has priority"
        ["module A where"
        ,"import qualified Data.ByteString as BS"
        ,"f = BS.read"
        ]
        (Position 2 10)
        [("readFile", CompletionItemKind_Function, "readFile", True, True, Nothing)]
        ],
      -- we need this test to make sure the ghcide completions module does not return completions for language pragmas. this functionality is turned on in hls
     completionTest
      "do not show pragma completions"
      [ "{-# LANGUAGE  ",
        "{module A where}",
        "main = return ()"
      ]
      (Position 0 13)
      []
  ]
  where
    brokenForWinGhc = knownBrokenOnWindows "Windows has strange things in scope for some reason"
    brokenForWinOldGhc =
      knownBrokenInSpecificEnv [HostOS Windows, GhcVer GHC96] "Windows (GHC == 9.6) has strange things in scope for some reason"
      . knownBrokenInSpecificEnv [HostOS Windows, GhcVer GHC98] "Windows (GHC == 9.8) has strange things in scope for some reason"

otherCompletionTests :: [TestTree]
otherCompletionTests = [
    completionTest
      "keyword"
      ["module A where", "f = newty"]
      (Position 1 9)
      [("newtype", CompletionItemKind_Keyword, "", False, False, Nothing)],
    completionTest
      "type context"
      [ "{-# OPTIONS_GHC -Wunused-binds #-}",
        "module A () where",
        "f = f",
        "g :: Intege"
      ]
      -- At this point the module parses but does not typecheck.
      -- This should be sufficient to detect that we are in a
      -- type context and only show the completion to the type.
      (Position 3 11)
      [("Integer", CompletionItemKind_Struct, "Integer", True, True, Nothing)],

    testSessionEmpty "duplicate record fields" $ do
      void $
        createDoc "B.hs" "haskell" $
          T.unlines
            [ "{-# LANGUAGE DuplicateRecordFields #-}",
              "module B where",
              "newtype Foo = Foo { member :: () }",
              "newtype Bar = Bar { member :: () }"
            ]
      docA <-
        createDoc "A.hs" "haskell" $
          T.unlines
            [ "module A where",
              "import B",
              "memb"
            ]
      _ <- waitForDiagnostics
      compls <- getCompletions docA $ Position 2 4
      let compls' = [txt | CompletionItem {_insertText = Just txt, ..} <- compls, _label == "member"]
      liftIO $ take 1 compls' @?= ["member"],

    testSessionEmpty "maxCompletions" $ do
        doc <- createDoc "A.hs" "haskell" $ T.unlines
            [ "{-# OPTIONS_GHC -Wunused-binds #-}",
                "module A () where",
                "a = Prelude."
            ]
        _ <- waitForDiagnostics
        compls <- getCompletions doc (Position 3 13)
        liftIO $ length compls @?= maxCompletions def
  ]

packageCompletionTests :: [TestTree]
packageCompletionTests =
  [ testSessionEmptyWithCradle "fromList" "cradle: {direct: {arguments: [-hide-all-packages, -package, base, A]}}" $ do

        doc <- createDoc "A.hs" "haskell" $ T.unlines
            [ "{-# OPTIONS_GHC -Wunused-binds #-}",
                "module A () where",
                "a = fromList"
            ]
        _ <- waitForDiagnostics
        compls <- getCompletions doc (Position 2 12)
        let compls' =
              [T.drop 1 $ T.dropEnd 3 d
              | CompletionItem {_documentation = Just (InR (MarkupContent MarkupKind_Markdown d)), _label}
                <- compls
              , _label == "fromList"
              ]
        liftIO $ take 3 (sort compls') @?=
          map ("Defined in "<>) [
                "'Data.List.NonEmpty"
              , "'GHC.Exts"
              , "'GHC.IsList"
              ]

  , testSessionEmptyWithCradle "Map" "cradle: {direct: {arguments: [-hide-all-packages, -package, base, -package, containers, A]}}" $ do
        doc <- createDoc "A.hs" "haskell" $ T.unlines
            [ "{-# OPTIONS_GHC -Wunused-binds #-}",
                "module A () where",
                "a :: Map"
            ]
        _ <- waitForDiagnostics
        compls <- getCompletions doc (Position 2 7)
        let compls' =
              [T.drop 1 $ T.dropEnd 3 d
              | CompletionItem {_documentation = Just (InR (MarkupContent MarkupKind_Markdown d)), _label}
                <- compls
              , _label == "Map"
              ]
        liftIO $ take 3 (sort compls') @?=
          map ("Defined in "<>)
              [ "'Data.Map"
              , "'Data.Map.Lazy"
              , "'Data.Map.Strict"
              ]
  , testSessionEmpty "no duplicates" $ do
        doc <- createDoc "A.hs" "haskell" $ T.unlines
            [ "{-# OPTIONS_GHC -Wunused-binds #-}",
                "module A () where",
                "import GHC.Exts(fromList)",
                "a = fromList"
            ]
        _ <- waitForDiagnostics
        compls <- getCompletions doc (Position 3 13)
        let duplicate =
              filter
                (\case
                  CompletionItem
                    { _insertText = Just "fromList"
                    , _documentation =
                      Just (InR (MarkupContent MarkupKind_Markdown d))
                    } ->
                    "GHC.Exts" `T.isInfixOf` d
                  _ -> False
                ) compls
        liftIO $ length duplicate @?= 1

  , testSessionEmpty "non-local before global" $ do
    -- non local completions are more specific
        doc <- createDoc "A.hs" "haskell" $ T.unlines
            [ "{-# OPTIONS_GHC -Wunused-binds #-}",
                "module A () where",
                "import GHC.Exts(fromList)",
                "a = fromList"
            ]
        _ <- waitForDiagnostics
        compls <- getCompletions doc (Position 3 13)
        let compls' =
              [_insertText
              | CompletionItem {_label, _insertText} <- compls
              , _label == "fromList"
              ]
        liftIO $ take 3 compls' @?=
          map Just ["fromList"]
  ]

projectCompletionTests :: [TestTree]
projectCompletionTests =
    [ testSessionEmptyWithCradle "from hiedb" "cradle: {direct: {arguments: [\"-Wmissing-signatures\", \"A\", \"B\"]}}" $ do
        _ <- createDoc "A.hs" "haskell" $ T.unlines
            [  "module A (anidentifier) where",
               "anidentifier = ()"
            ]
        _ <- waitForDiagnostics
        -- Note that B does not import A
        doc <- createDoc "B.hs" "haskell" $ T.unlines
            [ "module B where",
              "b = anidenti"
            ]
        compls <- getCompletions doc (Position 1 10)
        let compls' =
              [T.drop 1 $ T.dropEnd 3 d
              | CompletionItem {_documentation = Just (InR (MarkupContent MarkupKind_Markdown d)), _label}
                <- compls
              , _label == "anidentifier"
              ]
        liftIO $ compls' @?= ["Defined in 'A"],
      testSessionEmptyWithCradle "auto complete project imports" "cradle: {direct: {arguments: [\"-Wmissing-signatures\", \"ALocalModule\", \"B\"]}}" $ do
        _ <- createDoc "ALocalModule.hs" "haskell" $ T.unlines
            [  "module ALocalModule (anidentifier) where",
               "anidentifier = ()"
            ]
        _ <- waitForDiagnostics
        -- Note that B does not import A
        doc <- createDoc "B.hs" "haskell" $ T.unlines
            [ "module B where",
              "import ALocal"
            ]
        compls <- getCompletions doc (Position 1 13)
        let item = head $ filter ((== "ALocalModule") . (^. L.label)) compls
        liftIO $ do
          item ^. L.label @?= "ALocalModule",
      testSessionEmptyWithCradle "auto complete functions from qualified imports without alias" "cradle: {direct: {arguments: [\"-Wmissing-signatures\", \"A\", \"B\"]}}" $ do
        _ <- createDoc "A.hs" "haskell" $ T.unlines
            [  "module A (anidentifier) where",
               "anidentifier = ()"
            ]
        _ <- waitForDiagnostics
        doc <- createDoc "B.hs" "haskell" $ T.unlines
            [ "module B where",
              "import qualified A",
              "A."
            ]
        compls <- getCompletions doc (Position 2 2)
        let item = head compls
        liftIO $ do
          item ^. L.label @?= "anidentifier",
      testSessionEmptyWithCradle "auto complete functions from qualified imports with alias"
                  "cradle: {direct: {arguments: [\"-Wmissing-signatures\", \"A\", \"B\"]}}" $ do
        _ <- createDoc "A.hs" "haskell" $ T.unlines
            [  "module A (anidentifier) where",
               "anidentifier = ()"
            ]
        _ <- waitForDiagnostics
        doc <- createDoc "B.hs" "haskell" $ T.unlines
            [ "module B where",
              "import qualified A as Alias",
              "foo = Alias."
            ]
        compls <- getCompletions doc (Position 2 12)
        let item = head compls
        liftIO $ do
          item ^. L.label @?= "anidentifier"
    ]

completionDocTests :: [TestTree]
completionDocTests =
  [ testSessionEmpty "local define" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "foo = ()"
        , "bar = fo"
        ]
      let expected = "*Defined at line 2, column 1 in this module*\n"
      test doc (Position 2 8) "foo" Nothing [expected]
  , testSessionEmpty "local empty doc" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "foo = ()"
        , "bar = fo"
        ]
      test doc (Position 2 8) "foo" Nothing ["*Defined at line 2, column 1 in this module*\n"]
  , testSessionEmpty "local single line doc without newline" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "-- |docdoc"
        , "foo = ()"
        , "bar = fo"
        ]
      test doc (Position 3 8) "foo" Nothing ["*Defined at line 3, column 1 in this module*\n* * *\n\n\ndocdoc\n"]
  , testSessionEmpty "local multi line doc with newline" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "-- | abcabc"
        , "--"
        , "foo = ()"
        , "bar = fo"
        ]
      test doc (Position 4 8) "foo" Nothing ["*Defined at line 4, column 1 in this module*\n* * *\n\n\nabcabc\n"]
  , testSessionEmpty "local multi line doc without newline" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "-- |     abcabc"
        , "--"
        , "--def"
        , "foo = ()"
        , "bar = fo"
        ]
      test doc (Position 5 8) "foo" Nothing ["*Defined at line 5, column 1 in this module*\n* * *\n\n\nabcabc \n\ndef\n"]
  , testSessionEmpty "extern empty doc" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "foo = od"
        ]
      let expected = "*Imported from 'Prelude'*\n"
      test doc (Position 1 8) "odd" (Just $ T.length expected) [expected]
  ,  testSessionEmpty "extern single line doc without '\\n'" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "foo = no"
        ]
      let expected = "*Imported from 'Prelude'*\n* * *\n\n\nBoolean \"not\"\n"
      test doc (Position 1 8) "not" (Just $ T.length expected) [expected]
  ,  testSessionEmpty "extern mulit line doc" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "foo = i"
        ]
      let expected = "*Imported from 'Prelude'*\n* * *\n\n\nIdentity function. \n```haskell\nid x = x\n```\n"
      test doc (Position 1 7) "id" (Just $ T.length expected) [expected]
  , testSessionEmpty "extern defined doc" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "foo = i"
        ]
      let expected = "*Imported from 'Prelude'*\n"
      test doc (Position 1 7) "id" (Just $ T.length expected) [expected]
  ]
  where
    test doc pos label mn expected = do
      _ <- waitForDiagnostics
      compls <- getCompletions doc pos
      rcompls <- forM compls $ \item -> do
        rsp <- request SMethod_CompletionItemResolve item
        case rsp ^. L.result of
            Left err -> liftIO $ assertFailure ("completionItem/resolve failed with: " <> show err)
            Right x -> pure x
      let compls' = [
            -- We ignore doc uris since it points to the local path which determined by specific machines
            case mn of
                Nothing -> txt
                Just n  -> T.take n txt
            | CompletionItem {_documentation = Just (InR (MarkupContent MarkupKind_Markdown txt)), ..} <- rcompls
            , _label == label
            ]
      liftIO $ compls' @?= expected

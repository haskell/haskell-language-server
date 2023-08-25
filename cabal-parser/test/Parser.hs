{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Data.Maybe                 (isJust)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Distribution.Utils.Generic (safeHead)
import           System.FilePath            ((</>))
import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.HUnit
import           Text.Cabal.Parser
import           Text.Cabal.Types
import           Text.Megaparsec

parserTests :: FilePath -> TestTree
parserTests testDir =
  testGroup
    "CabalAST Parser Tests"
    [ parserTests' testDir
    ]

parserTests' :: FilePath -> TestTree
parserTests' testDir =
  testGroup
    "Parser Tests"
    [ testCase "hls cabal file" $ do
        ast <- callCabalParser (testDir </> "parser" </> "hls-real.cabal")
        let nsM = getStanzaWithTypeAndName "common" (Just "importLens") ast
        case nsM of
          Just (StanzaItem (Stanza _ (StanzaElements elems _) _)) ->
            assertBool "importLens contains conditional" $ any
            (\case
              (StanzaConditional _) -> True
              _                     -> False
            )
            elems
          _ -> assertFailure "no import lens common section"
    , testCase "cabal cabal file" $ do
        ast <- callCabalParser (testDir </> "parser" </> "cabal-real.cabal")
        let nsM = getStanzaWithTypeAndName "source-repository" (Just "head") ast
        assertBool "source repo stanza head exists" $ isJust nsM
    , testCase "haskell gi cabal file" $ do
        ast <- callCabalParser (testDir </> "parser" </> "haskellgi-real.cabal")
        let nsM = getStanzaWithTypeAndName "test-suite" (Just "doctests") ast
        case nsM of
          Just (StanzaItem (Stanza _ (StanzaElements elems _) _)) ->
            assertBool "test-suite doctests contains build-depends" $ any
            (\case
              (StanzaField (Field (KeyWord "build-depends:" _) _ _)) -> True
              _                                                      -> False
            )
            elems
          _ -> assertFailure "no test-suite doctests"
    , testCase "gi cairo connector cabal file" $ do
        _ast <- callCabalParser (testDir </> "parser" </> "gicc-real.cabal")
        pure ()
    , testCase "aeson cabal file" $ do
        ast <- callCabalParser (testDir </> "parser" </> "aeson-real.cabal")
        let nsM = getStanzaWithTypeAndName "library" Nothing ast
        case nsM of
          Just (StanzaItem (Stanza _ (StanzaElements elems _) _)) ->
            assertBool "library contains conditional" $ any
            (\case
              (StanzaConditional _) -> True
              _                     -> False
            )
            elems
          _ -> assertFailure "no library stanza"
        let nsM' = getStanzaWithTypeAndName "test-suite" (Just "aeson-tests") ast
        case nsM' of
          Just (StanzaItem (Stanza _ (StanzaElements elems _) _)) ->
            assertBool "test-suite contains conditional at very end" $ any
            (\case
              (StanzaConditional _) -> True
              _                     -> False
            )
            elems
          _ -> assertFailure "no test-suite aeson-tests stanza"
    , testCase "nested ifs" $ do
        ast <- callCabalParser (testDir </> "parser" </> "nested-ifs-real.cabal")
        let nsM = getStanzaWithTypeAndName "Library" Nothing ast
        case nsM of
          Just (StanzaItem (Stanza _ (StanzaElements elems _) _)) ->
            assertBool "library contains nested conditional" $ any
            (\case
              (StanzaConditional (Conditional _ (StanzaElements elems' _) _)) ->
                any (\case
                      (StanzaConditional _) -> True
                      _                     -> False)
                elems'
              _ -> False
            )
            elems
          _ -> assertFailure "no library stanza"
    , testCase "values in braces" $ do
        _ast <- callCabalParser (testDir </> "parser" </> "value-braces.cabal")
        pure ()
    , testCase "edit distance" $ do
        _ast <- callCabalParser (testDir </> "parser" </> "edit-dist.cabal")
        pure ()
    , testCase "stanza with fields in braces" $ do
        _ast <- callCabalParser (testDir </> "parser" </> "stanza-braces.cabal")
        pure ()
    , testCase "top level braces" $ do
        _ast <- callCabalParser (testDir </> "parser" </> "top-level-braces.cabal")
        pure ()
    , expectFailBecause "We do not support stanza elements directly after closing braces since it violates indentation rules" $ testCase "conditional braces - sameline" $ do
    _ast <- callCabalParser (testDir </> "parser" </> "nested-ifs-braces-simple.cabal")
    pure ()
    , testCase "conditional braces" $ do
    _ast <- callCabalParser (testDir </> "parser" </> "nested-ifs-braces-complex.cabal")
    pure ()
 , testCase "empty hs-source-dirs" $ do
        ast <- callCabalParser (testDir </> "parser" </> "empty-field.cabal")
        let nsM = getStanzaWithTypeAndName "library" Nothing ast
        case nsM of
          Just (StanzaItem (Stanza _ (StanzaElements elems _) _)) ->
            assertBool "library contains exposed-modules as field" $ any
              (\case
                (StanzaField (Field (KeyWord "exposed-modules:" _) _ _)) -> True
                _                     -> False
              )
            elems
          _ -> assertFailure "no library stanza"
    ]
  where
    callCabalParser :: FilePath -> IO CabalAST
    callCabalParser fp = do
      contents <- T.readFile fp
      case parseCabalFile fp contents of
        Left err -> do
          putStrLn $ errorBundlePretty err
          assertFailure "Must parse"
        Right ast -> do
          pure ast

    getStanzaWithTypeAndName :: T.Text -> Maybe T.Text -> CabalAST -> Maybe ASTItem
    getStanzaWithTypeAndName type' (Just name) (CabalAST items _) =
        safeHead $ filter
          (\case
            (StanzaItem (Stanza (StanzaDecl (StanzaType t _) sNameM _) _ _)) ->
              case sNameM of
                Just (StanzaName n _) -> n == name && t == type'
                _                     -> False
            _ -> False
          )
        items
    getStanzaWithTypeAndName type' Nothing (CabalAST items _) =
        safeHead $ filter
          (\case
            (StanzaItem (Stanza (StanzaDecl (StanzaType t _) sNameM _) _ _)) ->
              case sNameM of
                Nothing -> t == type'
                _       -> False
            _ -> False
          )
        items

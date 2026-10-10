{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens                        ((^.))
import qualified Data.ByteString                     as BS
import qualified Data.Map.Strict                     as Map
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Data.Text.Encoding                  (encodeUtf8)
import           Development.IDE                     (toNormalizedFilePath')
import           Development.IDE.Test                (expectDiagnostics)
import           Development.IDE.Types.Diagnostics   (FileDiagnostic (..))
import qualified Distribution.Fields                 as Syntax
import qualified Distribution.Parsec                 as Syntax
import           Ide.Plugin.Cabal.Completion.Types   (CabalPrefixInfo (..),
                                                      FieldContext (..),
                                                      StanzaContext (..))
import qualified Ide.Plugin.CabalProject             as CabalProject
import qualified Ide.Plugin.CabalProject.Data        as Data
import qualified Ide.Plugin.CabalProject.Diagnostics as Diagnostics
import qualified Ide.Plugin.CabalProject.Docs        as Docs
import qualified Language.LSP.Protocol.Lens          as L
import           Language.LSP.Protocol.Message       (TResponseMessage (..))
import qualified Language.LSP.Protocol.Message       as LSP
import           Language.LSP.Protocol.Types
import           Language.LSP.Test                   (getCompletions,
                                                      getDocumentSymbols,
                                                      getHover,
                                                      getSemanticTokens,
                                                      openDoc, request)
import           System.Directory                    (getCurrentDirectory)
import           System.FilePath
import           Test.Hls
import qualified Utils

main :: IO ()
main = defaultTestRunner tests

tests :: TestTree
tests =
  testGroup
    "cabal-project-plugin"
    [ diagnosticsUnitTests
    , contextUnitTests
    , docsUnitTests
    , sessionTests
    , completionTests
    , hoverTests
    , outlineTests
    , semanticTokensTests
    ]

-- ----------------------------------------------------------------
-- Pure unit tests
-- ----------------------------------------------------------------

parseFields :: Text -> IO [Syntax.Field Syntax.Position]
parseFields txt = case Syntax.readFields' (encodeUtf8 txt) of
  Left err          -> fail ("test snippet failed to parse: " <> show err)
  Right (fields, _) -> pure fields

checkProjectFieldsOf :: Text -> IO [Text]
checkProjectFieldsOf txt = do
  fields <- parseFields txt
  pure $ map (\fd -> fdLspDiagnostic fd ^. L.message) $
    Diagnostics.checkProjectFields (toNormalizedFilePath' "cabal.project") fields

diagnosticsUnitTests :: TestTree
diagnosticsUnitTests =
  testGroup
    "diagnostics (pure)"
    [ testCase "unknown top-level field" $ do
        msgs <- checkProjectFieldsOf "pakages: ./\n"
        msgs @?= ["Unknown field 'pakages' in cabal.project"]
    , testCase "unknown section" $ do
        msgs <- checkProjectFieldsOf "packages: ./\n\nfrobnicate foo\n  packages: ./\n"
        msgs @?= ["Unknown section 'frobnicate' in cabal.project"]
    , testCase "unknown field inside package stanza" $ do
        msgs <-
          checkProjectFieldsOf $
            "packages: ./\n\npackage *\n  frobnicate: True\n"
        msgs @?= ["Unknown field 'frobnicate' in section 'package'"]
    , testCase "conditional sections are traversed" $ do
        msgs <-
          checkProjectFieldsOf $
            "if impl(ghc >= 9.4)\n  active-repositories: hackage.haskell.org:merge\n"
        msgs @?= []
    , testCase "known fields and sections produce no warnings" $ do
        msgs <-
          checkProjectFieldsOf $
            "packages: ./\n\npackage *\n  tests: True\n\nsource-repository-package\n  type: git\n  location: https://example.com\n  subdir: .\n\nprogram-options\n  ghc-options: -Werror\n"
        msgs @?= []
    , testCase "isCabalProjectFile" $ do
        CabalProject.isCabalProjectFile "cabal.project" @?= True
        CabalProject.isCabalProjectFile "cabal.project.freeze" @?= True
        CabalProject.isCabalProjectFile "foo.cabal" @?= False
        CabalProject.isCabalProjectFile "other.project" @?= False
    , testCase "repo cabal.project has no unknown fields" $ do
        cwd <- getCurrentDirectory
        contents <- BS.readFile (cwd </> "cabal.project")
        case Syntax.readFields' contents of
          Left err -> fail ("repo cabal.project failed to parse: " <> show err)
          Right (fields, _) -> do
            let msgs = map (\fd -> fdLspDiagnostic fd ^. L.message) $
                  Diagnostics.checkProjectFields (toNormalizedFilePath' "cabal.project") fields
            msgs @?= []
    ]

-- ----------------------------------------------------------------
-- Completion context unit tests
-- ----------------------------------------------------------------

contextUnitTests :: TestTree
contextUnitTests =
  testGroup
    "completion context (pure)"
    [ testCase "top level" $ do
        fields <- parseFields "packages: ./\n"
        CabalProject.getProjectContext (prefixInfo 0 0) fields @?= (TopLevel, None)
    , testCase "top level keyword value" $ do
        fields <- parseFields "packages: ./\n"
        CabalProject.getProjectContext (prefixInfo 0 (T.length "packages:")) fields @?= (TopLevel, KeyWord "packages:")
    , testCase "inside package stanza" $ do
        fields <- parseFields "packages: ./\n\npackage mypkg\n  tests: True\n"
        CabalProject.getProjectContext (prefixInfo 3 2) fields @?= (Stanza "package" (Just "mypkg"), None)
    , testCase "stanza field value" $ do
        fields <- parseFields "packages: ./\n\npackage mypkg\n  ghc-options: -j4\n"
        CabalProject.getProjectContext (prefixInfo 3 (T.length "  ghc-options:")) fields @?= (Stanza "package" (Just "mypkg"), KeyWord "ghc-options:")
    , testCase "inside source-repository-package" $ do
        fields <- parseFields "source-repository-package\n  type: git\n"
        CabalProject.getProjectContext (prefixInfo 1 2) fields @?= (Stanza "source-repository-package" Nothing, None)
    ]

prefixInfo :: UInt -> Int -> CabalPrefixInfo
prefixInfo line col =
  CabalPrefixInfo
    { completionPrefix = ""
    , isStringNotation = Nothing
    , completionCursorPosition = Position line (fromIntegral col)
    , completionRange = Range (Position line (fromIntegral col)) (Position line (fromIntegral col))
    , completionWorkingDir = ""
    , completionFileName = "cabal.project"
    }

-- ----------------------------------------------------------------
-- Session tests
-- ----------------------------------------------------------------

sessionTests :: TestTree
sessionTests =
  testGroup
    "diagnostics (session)"
    [ Utils.runProjectTestCaseSession "warns on unknown field" "unknown-field" $ do
        _ <- openDoc "cabal.project" "cabalproject"
        expectDiagnostics
          [ ( "cabal.project"
            , [(DiagnosticSeverity_Warning, (0, 0), "Unknown field 'pakages'", Nothing :: Maybe Text)]
            )
          ]
    , Utils.runProjectTestCaseSession "warns on unknown section" "unknown-section" $ do
        _ <- openDoc "cabal.project" "cabalproject"
        expectDiagnostics
          [ ( "cabal.project"
            , [(DiagnosticSeverity_Warning, (2, 0), "Unknown section 'frobnicate'", Nothing :: Maybe Text)]
            )
          ]
    , Utils.runProjectTestCaseSession "errors on parse failure" "parse-error" $ do
        _ <- openDoc "cabal.project" "cabalproject"
        expectDiagnostics
          [ ( "cabal.project"
            , [(DiagnosticSeverity_Error, (0, 0), "Failed to parse project file", Nothing :: Maybe Text)]
            )
          ]
    , Utils.runProjectTestCaseSession "no diagnostics for valid project file" "valid" $ do
        doc <- openDoc "cabal.project" "cabalproject"
        expectNoMoreDiagnostics 2 doc "cabal.project"
    , Utils.runProjectTestCaseSession "language id 'cabal' also triggers diagnostics" "unknown-field" $ do
        _ <- openDoc "cabal.project" "cabal"
        expectDiagnostics
          [ ( "cabal.project"
            , [(DiagnosticSeverity_Warning, (0, 0), "Unknown field 'pakages'", Nothing :: Maybe Text)]
            )
          ]
    , Utils.runProjectTestCaseSession "no diagnostics for freeze file" "freeze" $ do
        doc <- openDoc "cabal.project.freeze" "cabalproject"
        expectNoMoreDiagnostics 2 doc "cabal.project.freeze"
    ]

-- ----------------------------------------------------------------
-- Completion tests
-- ----------------------------------------------------------------

completionTests :: TestTree
completionTests =
  testGroup
    "completions (session)"
    [ Utils.runProjectTestCaseSession "top level completions" "unknown-section" $ do
        doc <- openDoc "cabal.project" "cabalproject"
        compls <- getCompletions doc (Position 0 4)
        let labels = map (^. L.label) compls
        liftIO $ do
          assertBool "missing 'packages:'" ("packages:" `elem` labels)
          assertBool "missing 'optional-packages:'" ("optional-packages:" `elem` labels)
          assertBool "missing section 'package'" ("package" `elem` labels)
          assertBool "missing section 'source-repository-package'" ("source-repository-package" `elem` labels)
          assertBool "unexpected cabal field 'exposed-modules:'" ("exposed-modules:" `notElem` labels)
    , Utils.runProjectTestCaseSession "boolean value completions" "valid" $ do
        doc <- openDoc "cabal.project" "cabalproject"
        -- cursor at start of 'True' on the 'tests: True' line
        compls <- getCompletions doc (Position 4 9)
        let labels = map (^. L.label) compls
        liftIO $ do
          assertBool "missing 'True'" ("True" `elem` labels)
          assertBool "missing 'False'" ("False" `elem` labels)
    , Utils.runProjectTestCaseSession "source-repository-package field completions" "srp" $ do
        doc <- openDoc "cabal.project" "cabalproject"
        -- empty last line inside the source-repository-package section
        compls <- getCompletions doc (Position 5 2)
        let labels = map (^. L.label) compls
        liftIO $ do
          assertBool "missing 'type:'" ("type:" `elem` labels)
          assertBool "missing 'location:'" ("location:" `elem` labels)
          assertBool "missing 'tag:'" ("tag:" `elem` labels)
          assertBool "missing 'subdir:'" ("subdir:" `elem` labels)
    , Utils.runProjectTestCaseSession "filepath completions" "filepath" $ do
        doc <- openDoc "cabal.project" "cabalproject"
        compls <- getCompletions doc (Position 0 (fromIntegral (T.length "packages: ") :: UInt))
        let labels = map (^. L.label) compls
        liftIO $ do
          assertBool "missing directory 'pkgs'" (any ("pkgs" `T.isPrefixOf`) labels)
          assertBool "missing file 'cabal.project'" ("cabal.project" `elem` labels)
    ]

-- ----------------------------------------------------------------
-- Semantic token tests
-- ----------------------------------------------------------------

-- | Decodes the LSP-encoded token array (5 UInts per token) back into
-- absolutely-positioned tokens.
decodeSemanticTokensArray :: SemanticTokensLegend -> [UInt] -> [(UInt, UInt, UInt, Text)]
decodeSemanticTokensArray legend encoded =
  let SemanticTokensLegend legendTypes _legendMods = legend
      go :: UInt -> UInt -> [UInt] -> [(UInt, UInt, UInt, Text)]
      go _ _ [] = []
      go lastLine lastChar (dl : dc : len : ty : _mods : rest) =
        let l = lastLine + dl
            c = if dl == 0 then lastChar + dc else dc
         in (l, c, len, legendTypes !! fromIntegral ty) : go l c rest
      go _ _ _ = []
   in go 0 0 encoded

semanticTokensTests :: TestTree
semanticTokensTests =
  Utils.runProjectTestCaseSession "semantic tokens" "valid" $ do
    doc <- openDoc "cabal.project" "cabalproject"
    InL (SemanticTokens _ encoded) <- getSemanticTokens doc
    let toks = decodeSemanticTokensArray defaultSemanticTokensLegend encoded
        hasExpected (l, ch, len, ty) =
          any
            (\(tl, tc, tlen, tty) -> (tl, tc, tlen, tty) == (fromIntegral l, fromIntegral ch, fromIntegral len, toEnumBaseType ty))
            toks
    liftIO $ do
      assertBool "expected 'packages' property token" (hasExpected (0, 0, 8, SemanticTokenTypes_Property))
      assertBool "expected 'package' keyword token" (hasExpected (2, 0, 7, SemanticTokenTypes_Keyword))
      assertBool "expected 'True' keyword token" (hasExpected (4, 9, 4, SemanticTokenTypes_Keyword))
      assertBool "expected comment token" (hasExpected (17, 0, 23, SemanticTokenTypes_Comment))

-- ----------------------------------------------------------------
-- Docs / hover unit tests (pure)
-- ----------------------------------------------------------------

-- | Every completer key must have hover documentation, otherwise a future
-- 'Data.hs' addition silently hovers to null.
docsUnitTests :: TestTree
docsUnitTests =
  testGroup
    "docs (pure)"
    [ testCase "all top-level fields documented" $ do
        let missing = filter (not . (`Map.member` Docs.projectFieldDocs)) (Map.keys Data.projectTopLevelFields)
        missing @?= []
    , testCase "all stanza fields documented" $ do
        let missing =
              [ kw
              | stanzaMap <- Map.elems Data.projectStanzaKeywordMap
              , kw <- Map.keys stanzaMap
              , not (Map.member kw Docs.projectFieldDocs)
              ]
        missing @?= []
    , testCase "all sections documented" $ do
        let missing = filter (not . (`Map.member` Docs.projectSectionDocs)) Data.projectSectionNames
        missing @?= []
    -- reverse direction: every documented field/section must be accepted by
    -- the diagnostics tables, otherwise hovering works but the field is
    -- flagged as unknown.
    , testCase "all documented fields are accepted by diagnostics" $ do
        let stanzaKeys = concatMap Map.keys (Map.elems Data.projectStanzaKeywordMap)
            unknown =
              [ kw
              | kw <- Map.keys Docs.projectFieldDocs
              , kw `notElem` Map.keys Data.projectTopLevelFields
              , kw `notElem` stanzaKeys
              ]
        unknown @?= []
    , testCase "all documented sections are known sections" $ do
        filter (`notElem` Data.projectSectionNames) (Map.keys Docs.projectSectionDocs) @?= []
    , testCase "hover on top-level field name" $ do
        fields <- parseFields "packages: ./\n"
        (Just (markup, rng)) <- pure $ Docs.projectHover (Syntax.Position 1 3) fields
        assertBool "summary missing" ("Project packages." `T.isInfixOf` markup)
        assertBool "link missing" ("cfg-field-packages" `T.isInfixOf` markup)
        rng ^. L.start . L.line @?= 0
        rng ^. L.start . L.character @?= 0
    , testCase "no hover on field value" $ do
        fields <- parseFields "packages: ./\n"
        Docs.projectHover (Syntax.Position 1 12) fields @?= Nothing
    , testCase "hover on section and stanza field" $ do
        fields <- parseFields "source-repository-package\n  type: git\n"
        (Just (secMarkup, _)) <- pure $ Docs.projectHover (Syntax.Position 1 4) fields
        assertBool "section tag missing" ("(section)" `T.isInfixOf` secMarkup)
        (Just (fieldMarkup, _)) <- pure $ Docs.projectHover (Syntax.Position 2 4) fields
        assertBool "field anchor missing" ("cfg-field-type" `T.isInfixOf` fieldMarkup)
    , testCase "unknown section hovers to Nothing" $ do
        fields <- parseFields "frobnicate foo\n  packages: ./\n"
        Docs.projectHover (Syntax.Position 1 2) fields @?= Nothing
    , testCase "hover on conditional and fallback field" $ do
        fields <- parseFields "if impl(ghc >= 9.4)\n  active-repositories: x\n"
        (Just (ifMarkup, _)) <- pure $ Docs.projectHover (Syntax.Position 1 2) fields
        assertBool "`if` missing" ("`if`" `T.isInfixOf` ifMarkup)
        (Just (kwMarkup, _)) <- pure $ Docs.projectHover (Syntax.Position 2 4) fields
        assertBool "anchor link missing" ("cfg-field-active-repositories" `T.isInfixOf` kwMarkup)
    ]

-- ----------------------------------------------------------------
-- Hover tests (session)
-- ----------------------------------------------------------------

hoverTests :: TestTree
hoverTests =
  testGroup
    "hover (session)"
    [ Utils.runProjectTestCaseSession "hover on field name" "valid" $ do
        doc <- openDoc "cabal.project" "cabalproject"
        hover <- getHover doc (Position 0 2)
        liftIO $ case hover of
          Just h | InL (MarkupContent _ value) <- h ^. L.contents -> do
            assertBool "summary missing" ("Project packages." `T.isInfixOf` value)
            assertBool "link missing" ("cabal-project-description-file.html#cfg-field-packages" `T.isInfixOf` value)
          _ -> assertFailure ("expected hover contents, got: " <> show hover)
    , Utils.runProjectTestCaseSession "no hover on field value" "valid" $ do
        doc <- openDoc "cabal.project" "cabalproject"
        hover <- getHover doc (Position 4 10)
        liftIO $ hover @?= Nothing
    , Utils.runProjectTestCaseSession "hover on conditional" "valid" $ do
        doc <- openDoc "cabal.project" "cabalproject"
        hover <- getHover doc (Position 8 1)
        liftIO $ case hover of
          Just h | InL (MarkupContent _ value) <- h ^. L.contents ->
            assertBool "conditional name missing" ("`if`" `T.isInfixOf` value)
          _ -> assertFailure ("expected hover contents, got: " <> show hover)
    , Utils.runProjectTestCaseSession "documentHighlight returns null" "valid" $ do
        doc <- openDoc "cabal.project" "cabalproject"
        TResponseMessage _ _ res <- request LSP.SMethod_TextDocumentDocumentHighlight (DocumentHighlightParams (doc) (Position 0 2) Nothing Nothing)
        liftIO $ res @?= Right (InR Null)
    , Utils.runProjectTestCaseSession "signatureHelp returns null" "valid" $ do
        doc <- openDoc "cabal.project" "cabalproject"
        TResponseMessage _ _ res <- request LSP.SMethod_TextDocumentSignatureHelp (SignatureHelpParams (doc) (Position 0 2) Nothing Nothing)
        liftIO $ res @?= Right (InR Null)
    ]

-- ----------------------------------------------------------------
-- Outline tests (session)
-- ----------------------------------------------------------------

outlineTests :: TestTree
outlineTests =
  Utils.runProjectTestCaseSession "document symbols" "srp" $ do
    doc <- openDoc "cabal.project" "cabalproject"
    symbols <- getDocumentSymbols doc
    liftIO $ case symbols of
      Right syms -> do
        let names = map (^. L.name) syms
            childrenNames = concatMap (maybe [] (map (^. L.name)) . (^. L.children)) syms
        assertBool "missing 'packages'" ("packages" `elem` names)
        assertBool "missing 'source-repository-package'" (any ("source-repository-package" `T.isPrefixOf`) names)
        assertBool "missing 'type' child" ("type" `elem` childrenNames)
        assertBool "missing 'location' child" ("location" `elem` childrenNames)
      Left _ -> assertFailure "expected hierarchical document symbols"

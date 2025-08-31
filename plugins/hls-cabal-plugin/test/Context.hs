{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE QuasiQuotes              #-}

module Context where

import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as Text
import           Development.IDE.Plugin.Completions.Types    (PosPrefixInfo (..))
import           Ide.Plugin.Cabal
import           Ide.Plugin.Cabal.Completion.Completer.Paths
import           Ide.Plugin.Cabal.Completion.Completions
import           Ide.Plugin.Cabal.Completion.Types           (Context,
                                                              FieldContext (KeyWord, None),
                                                              StanzaContext (Stanza, TopLevel))
import qualified Ide.Plugin.Cabal.Parse                      as Parse
import           Test.Hls
import           Utils                                       as T

cabalPlugin :: PluginTestDescriptor Ide.Plugin.Cabal.Log
cabalPlugin = mkPluginTestDescriptor descriptor "cabal context"

contextTests :: TestTree
contextTests =
    testGroup
        "Context Tests"
        [ pathCompletionInfoFromCompletionContextTests
        , getContextTests
        ]

pathCompletionInfoFromCompletionContextTests :: TestTree
pathCompletionInfoFromCompletionContextTests =
    testGroup
        "Completion Info to Completion Context Tests"
        [ testCase "Current Directory - no leading ./ by default" $ do
            let complInfo = pathCompletionInfoFromCabalPrefixInfo "" $ simpleCabalPrefixInfoFromFp "" testDataDir
            queryDirectory complInfo @?= ""
        , testCase "Current Directory - partly written next" $ do
            let complInfo = pathCompletionInfoFromCabalPrefixInfo "" $ simpleCabalPrefixInfoFromFp "di" testDataDir
            queryDirectory complInfo @?= ""
            pathSegment complInfo @?= "di"
        , testCase "Current Directory - alternative writing" $ do
            let complInfo = pathCompletionInfoFromCabalPrefixInfo "" $ simpleCabalPrefixInfoFromFp "./" testDataDir
            queryDirectory complInfo @?= "./"
        , testCase "Subdirectory" $ do
            let complInfo = pathCompletionInfoFromCabalPrefixInfo "" $ simpleCabalPrefixInfoFromFp "dir1/" testDataDir
            queryDirectory complInfo @?= "dir1/"
            pathSegment complInfo @?= ""
        , testCase "Subdirectory - partly written next" $ do
            let complInfo = pathCompletionInfoFromCabalPrefixInfo "" $ simpleCabalPrefixInfoFromFp "dir1/d" testDataDir
            queryDirectory complInfo @?= "dir1/"
            pathSegment complInfo @?= "d"
        , testCase "Subdirectory - partly written next" $ do
            let complInfo = pathCompletionInfoFromCabalPrefixInfo "" $ simpleCabalPrefixInfoFromFp "dir1/dir2/d" testDataDir
            queryDirectory complInfo @?= "dir1/dir2/"
            pathSegment complInfo @?= "d"
        ]

getContextTests :: TestTree
getContextTests =
    testGroup
        "Context Tests Real"
        [ testCase "Empty File - Start" $ do
            -- for a completely empty file, the context needs to
            -- be top level without a specified keyword
            ctx <- callGetContext (Position 0 0) "" ""
            ctx @?= (TopLevel, None)
        , testCase "Cabal version keyword - no value, no space after :" $ do
            -- on a file, where the keyword is already written
            -- the context should still be toplevel but the keyword should be recognized
            ctx <- callGetContext (Position 0 14) "" "cabal-version:\n"
            ctx @?= (TopLevel, KeyWord "cabal-version:")
        , testCase "Cabal version keyword - cursor in keyword" $ do
            -- on a file, where the keyword is already written
            -- but the cursor is in the middle of the keyword,
            -- we are not in a keyword context
            ctx <- callGetContext (Position 0 5) "cabal" "cabal-version:\n"
            ctx @?= (TopLevel, None)
        , testCase "Cabal version keyword - no value, many spaces" $ do
            -- on a file, where the "cabal-version:" keyword is already written
            -- the context should still be top level but the keyword should be recognized
            ctx <- callGetContext (Position 0 45) "" ("cabal-version:" <> T.replicate 50 " " <> "\n")
            ctx @?= (TopLevel, KeyWord "cabal-version:")
        , testCase "Cabal version keyword - keyword partly written" $ do
            -- in the first line of the file, if the keyword
            -- has not been written completely, the keyword context
            -- should still be None
            ctx <- callGetContext (Position 0 5) "cabal" "cabal"
            ctx @?= (TopLevel, None)
        , testCase "Cabal version keyword - value partly written" $ do
            -- in the first line of the file, if the keyword
            -- has not been written completely, the keyword context
            -- should still be None
            ctx <- callGetContext (Position 0 17) "1." "cabal-version: 1."
            ctx @?= (TopLevel, KeyWord "cabal-version:")
        , testCase "Inside Stanza - no keyword" $ do
            -- on a file, where the library stanza has been defined
            -- but no keyword is defined afterwards, the stanza context should be recognized
            ctx <- callGetContext (Position 3 2) "" libraryStanzaData
            ctx @?= (Stanza "library" Nothing, None)
        , testCase "Inside Stanza - keyword, no value" $ do
            -- on a file, where the library stanza and a keyword
            -- has been defined, the keyword and stanza should be recognized
            ctx <- callGetContext (Position 4 21) "" libraryStanzaData
            ctx @?= (Stanza "library" Nothing, KeyWord "build-depends:")
        , testCase "Cabal version keyword - no value, next line" $ do
            -- if the cabal version keyword has been written but without a value,
            -- in the next line we still should be in top level context with no keyword
            -- since the cabal version keyword and value pair need to be in the same line.
            -- However, that's too much work to implement for virtually no benefit, so we
            -- test here the status-quo is satisfied.
            ctx <- callGetContext (Position 1 2) "" "cabal-version:\n\n"
            ctx @?= (TopLevel, KeyWord "cabal-version:")
        , testCase "Non-cabal-version keyword - no value, next line indented position" $ do
            -- if a keyword, other than the cabal version keyword has been written
            -- with no value, in the next line we still should be in top level keyword context
            -- of the keyword with no value, since its value may be written in the next line
            ctx <- callGetContext (Position 2 4) "" topLevelData
            ctx @?= (TopLevel, KeyWord "name:")
        , testCase "Non-cabal-version keyword - no value, next line at start" $ do
            -- if a keyword, other than the cabal version keyword has been written
            -- with no value, in the next line we still should be in top level context
            -- but not the keyword's, since it is not viable to write a value for a
            -- keyword a the start of the next line
            ctx <- callGetContext (Position 2 0) "" topLevelData
            ctx @?= (TopLevel, None)
        , testCase "Toplevel after stanza partially written" $ do
            ctx <- callGetContext (Position 6 2) "ma" libraryStanzaData
            ctx @?= (TopLevel, None)
        , testCase "Non-cabal-version keyword - no value, multiple lines between" $ do
            -- if a keyword, other than the cabal version keyword has been written
            -- with no value, even with multiple lines in between we can still write the
            -- value corresponding to the keyword
            ctx <- callGetContext (Position 5 4) "" topLevelData
            ctx @?= (TopLevel, KeyWord "name:")
        , testCase "Keyword inside stanza - cursor indented more than keyword in next line" $ do
            -- if a keyword, other than the cabal version keyword has been written
            -- in a stanza context with no value, then the value may be written in the next line,
            -- when the cursor is indented more than the keyword
            ctx <- callGetContext (Position 5 8) "" libraryStanzaData
            ctx @?= (Stanza "library" Nothing, KeyWord "build-depends:")
        , testCase "Keyword inside stanza - cursor indented less than keyword in next line" $ do
            -- if a keyword, other than the cabal version keyword has been written
            -- in a stanza context with no value, then the value may not be written in the next line,
            -- when the cursor is indented less than the keyword
            ctx <- callGetContext (Position 5 2) "" libraryStanzaData
            ctx @?= (Stanza "library" Nothing, None)
        , testCase "Keyword inside stanza - cursor at start of next line" $ do
            -- in a stanza context with no value the value may not be written in the next line,
            -- when the cursor is not indented and we are in the top level context
            ctx <- callGetContext (Position 5 0) "" libraryStanzaData
            ctx @?= (TopLevel, None)
        , testCase "Top level - cursor in later line with partially written value" $ do
            ctx <- callGetContext (Position 5 13) "eee" topLevelData
            ctx @?= (TopLevel, KeyWord "name:")
        , testCase "If is ignored" $ do
            ctx <- callGetContext (Position 5 18) "" conditionalData
            ctx @?= (Stanza "library" Nothing, None)
        , testCase "Elif is ignored" $ do
            ctx <- callGetContext (Position 7 18) "" conditionalData
            ctx @?= (Stanza "library" Nothing, None)
        , testCase "Else is ignored" $ do
            ctx <- callGetContext (Position 9 18) "" conditionalData
            ctx @?= (Stanza "library" Nothing, KeyWord "buildable:")
        , testCase "Named Stanza" $ do
            ctx <- callGetContext (Position 2 18) "" executableStanzaData
            ctx @?= (TopLevel, None)
        , testCase "Multi line, finds context in same line" $ do
            ctx <- callGetContext (Position 5 18) "" multiLineOptsData
            ctx @?= (Stanza "library" Nothing, KeyWord "build-depends:")
        , testCase "Multi line, in the middle of option" $ do
            ctx <- callGetContext (Position 6 11) "" multiLineOptsData
            ctx @?= (Stanza "library" Nothing, KeyWord "build-depends:")
        , testCase "Multi line, finds context in between lines" $ do
            ctx <- callGetContext (Position 7 8) "" multiLineOptsData
            ctx @?= (Stanza "library" Nothing, KeyWord "build-depends:")
        , testCase "Multi line, finds context in between lines, start if line" $ do
            ctx <- callGetContext (Position 7 0) "" multiLineOptsData
            ctx @?= (TopLevel, None)
        , testCase "Multi line, end of option" $ do
            ctx <- callGetContext (Position 8 14) "" multiLineOptsData
            ctx @?= (Stanza "library" Nothing, KeyWord "build-depends:")
        , parameterisedCursorTest "Contexts in large testfile" multiPositionTestData
            [ (TopLevel, None)
            , (TopLevel, KeyWord "cabal-version:")
            , (TopLevel, None)
            , (TopLevel, KeyWord "description:")
            , (TopLevel, KeyWord "extra-source-files:")
            , (TopLevel, None)
            -- this might not be what we want, maybe add another Context
            , (TopLevel, None)
            -- this might not be what we want, maybe add another Context
            , (TopLevel, None)
            , (Stanza "source-repository" (Just "head"), None)
            , (Stanza "source-repository" (Just "head"), KeyWord "type:")
            , (Stanza "source-repository" (Just "head"), KeyWord "type:")
            , (Stanza "source-repository" (Just "head"), KeyWord "type:")
            , (Stanza "source-repository" (Just "head"), None)
            , (Stanza "common" (Just "cabalfmt"), None)
            , (Stanza "common" (Just "cabalfmt"), None)
            , (Stanza "common" (Just "cabalfmt"), KeyWord "build-depends:")
            ]
            $ \fileContent posPrefInfo ->
                callGetContext (cursorPos posPrefInfo) (prefixText posPrefInfo) fileContent
        ]
  where
    callGetContext :: Position -> T.Text -> T.Text -> IO Context
    callGetContext pos pref ls = do
        case Parse.readCabalFields "not-real" (Text.encodeUtf8 ls) of
            Left err -> fail $ show err
            Right fields -> do
                getContext mempty (simpleCabalPrefixInfoFromPos pos pref) fields

-- ------------------------------------------------------------------------
-- Test Data
-- ------------------------------------------------------------------------

libraryStanzaData :: T.Text
libraryStanzaData = [__i|
    cabal-version:      3.0
    name:               simple-cabal
    library
        default-language: Haskell98
        build-depends:

    ma
|]

executableStanzaData :: T.Text
executableStanzaData = [__i|
    cabal-version:      3.0
    name:               simple-cabal
    executable exeName
        default-language: Haskell2010
        hs-source-dirs: test/preprocessor
|]

topLevelData :: T.Text
topLevelData = [__i|
    cabal-version:      3.0
    name:



              eee
|]

conditionalData :: T.Text
conditionalData = [__i|
    cabal-version:      3.0
    name:               simple-cabal
    library
        if os(windows)
           buildable:
        elif os(linux)
           buildable:
        else
           buildable:
|]
multiLineOptsData :: T.Text
multiLineOptsData = [__i|
    cabal-version:      3.0
    name:


    library
        build-depends:
            base,

            text ,
|]

multiPositionTestData :: T.Text
multiPositionTestData = [__i|
    cabal-version:      3.4
           ^             ^
    category:           Development
    ^
    name:               haskell-language-server
    description:
      Please see the README on GitHub at <https://github.com/haskell/haskell-language-server\#readme>
        ^
    extra-source-files:
      README.md
      ChangeLog.md
      test/testdata/**/*.project
      test/testdata/**/*.cabal
      test/testdata/**/*.yaml
      test/testdata/**/*.hs
      test/testdata/**/*.json
        ^
      -- These globs should only match test/testdata
      plugins/**/*.project

    source-repository head
         ^              ^   ^
      type:     git
        ^    ^    ^  ^
      location: https://github.com/haskell/haskell-language-server

      ^
    common cabalfmt

      ^
      build-depends: haskell-language-server:hls-cabal-fmt-plugin
        ^            ^
      cpp-options: -Dhls_cabalfmt
|]

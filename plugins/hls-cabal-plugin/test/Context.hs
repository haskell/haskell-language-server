{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}

module Context where

import           Control.Monad.Trans.Maybe                   (runMaybeT)
import qualified Data.Text                                   as T
import qualified Data.Text.Utf16.Rope.Mixed                  as Rope
import           Ide.Plugin.Cabal
import           Ide.Plugin.Cabal.Completion.Completer.Paths
import           Ide.Plugin.Cabal.Completion.Completions
import           Ide.Plugin.Cabal.Completion.Types           (Context,
                                                              FieldContext (KeyWord, None),
                                                              StanzaContext (Stanza, TopLevel))
import           Test.Hls
import           Utils                                       as T

cabalPlugin :: PluginTestDescriptor Ide.Plugin.Cabal.Log
cabalPlugin = mkPluginTestDescriptor descriptor "cabal context"

contextTests :: TestTree
contextTests =
    testGroup
        "Context Tests "
        [ pathCompletionInfoFromCompletionContextTests
        , getContextTests
        ]

pathCompletionInfoFromCompletionContextTests :: TestTree
pathCompletionInfoFromCompletionContextTests =
    testGroup
        "Completion Info to Completion Context Tests"
        [ testCase "Current Directory" $ do
            let complInfo = pathCompletionInfoFromCabalPrefixInfo "" $ simpleCabalPrefixInfoFromFp "" testDataDir
            queryDirectory complInfo @?= "./"
        , testCase "Current Directory - partly written next" $ do
            let complInfo = pathCompletionInfoFromCabalPrefixInfo "" $ simpleCabalPrefixInfoFromFp "di" testDataDir
            queryDirectory complInfo @?= "./"
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
        "Context Tests"
        [ testCase "Empty File - Start" $ do
            -- for a completely empty file, the context needs to
            -- be top level without a specified keyword
            ctx <- callGetContext (Position 0 0) "" [""]
            ctx @?= (TopLevel, None)
        , testCase "Cabal version keyword - no value, no space after :" $ do
            -- on a file, where the keyword is already written
            -- the context should still be toplevel but the keyword should be recognized
            ctx <- callGetContext (Position 0 14) "" ["cabal-version:"]
            ctx @?= (TopLevel, KeyWord "cabal-version:")
        , testCase "Cabal version keyword - cursor in keyword" $ do
            -- on a file, where the keyword is already written
            -- but the cursor is in the middle of the keyword,
            -- we are not in a keyword context
            ctx <- callGetContext (Position 0 5) "cabal" ["cabal-version:"]
            ctx @?= (TopLevel, None)
        , testCase "Cabal version keyword - no value, many spaces" $ do
            -- on a file, where the "cabal-version:" keyword is already written
            -- the context should still be top level but the keyword should be recognized
            ctx <- callGetContext (Position 0 45) "" ["cabal-version:" <> T.replicate 50 " "]
            ctx @?= (TopLevel, KeyWord "cabal-version:")
        , testCase "Cabal version keyword - keyword partly written" $ do
            -- in the first line of the file, if the keyword
            -- has not been written completely, the keyword context
            -- should still be None
            ctx <- callGetContext (Position 0 5) "cabal" ["cabal"]
            ctx @?= (TopLevel, None)
        , testCase "Cabal version keyword - value partly written" $ do
            -- in the first line of the file, if the keyword
            -- has not been written completely, the keyword context
            -- should still be None
            ctx <- callGetContext (Position 0 17) "1." ["cabal-version: 1."]
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
        , expectFailBecause "While not valid, it is not that important to make the code more complicated for this" $
            testCase "Cabal version keyword - no value, next line" $ do
                -- if the cabal version keyword has been written but without a value,
                -- in the next line we still should be in top level context with no keyword
                -- since the cabal version keyword and value pair need to be in the same line
                ctx <- callGetContext (Position 1 2) "" ["cabal-version:", ""]
                ctx @?= (TopLevel, None)
        , testCase "Non-cabal-version keyword - no value, next line indentented position" $ do
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
        , testCase "Named Stanza" $ do
            ctx <- callGetContext (Position 2 18) "" executableStanzaData
            ctx @?= (Stanza "executable" (Just "exeName"), None)
        ]
  where
    callGetContext :: Position -> T.Text -> [T.Text] -> IO Context
    callGetContext pos pref ls = do
        runMaybeT (getContext mempty (simpleCabalPrefixInfoFromPos pos pref) (Rope.fromText $ T.unlines ls))
            >>= \case
                Nothing -> assertFailure "Context must be found"
                Just ctx -> pure ctx

-- ------------------------------------------------------------------------
-- Test Data
-- ------------------------------------------------------------------------

libraryStanzaData :: [T.Text]
libraryStanzaData =
    [ "cabal-version:      3.0"
    , "name:               simple-cabal"
    , "library "
    , "    default-language: Haskell98"
    , "    build-depends:    "
    , "           "
    , "ma  "
    ]

executableStanzaData :: [T.Text]
executableStanzaData =
    [ "cabal-version:      3.0"
    , "name:               simple-cabal"
    , "executable exeName"
    , "    default-language: Haskell2010"
    , "    hs-source-dirs: test/preprocessor"
    ]

topLevelData :: [T.Text]
topLevelData =
    [ "cabal-version:      3.0"
    , "name:"
    , ""
    , ""
    , ""
    , "          eee"
    ]

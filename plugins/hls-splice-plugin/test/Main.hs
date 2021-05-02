{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module Main (main) where

import           Control.Monad
import           Data.List               (find)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import qualified Ide.Plugin.Splice       as Splice
import           Ide.Plugin.Splice.Types
import           System.Directory
import           System.FilePath
import           System.Time.Extra       (sleep)
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

plugin :: PluginDescriptor IdeState
plugin = Splice.descriptor "splice"

tests :: TestTree
tests =
    testGroup
        "splice"
        [ goldenTest "TSimpleExp.hs" Inplace 6 15
        , goldenTest "TSimpleExp.hs" Inplace 6 24
        , goldenTest "TTypeAppExp.hs" Inplace 7 5
        , goldenTest "TErrorExp.hs" Inplace 6 15
        , goldenTest "TErrorExp.hs" Inplace 6 51
        , goldenTest "TQQExp.hs" Inplace 6 17
        , goldenTest "TQQExp.hs" Inplace 6 25
        , goldenTest "TQQExpError.hs" Inplace 6 13
        , goldenTest "TQQExpError.hs" Inplace 6 22
        , testGroup "Pattern Splices"
            [ goldenTest "TSimplePat.hs" Inplace 6 3
            , goldenTest "TSimplePat.hs" Inplace 6 22
            , goldenTest "TSimplePat.hs" Inplace 6 3
            , goldenTest "TSimplePat.hs" Inplace 6 22
            , goldenTest "TErrorPat.hs" Inplace 6 3
            , goldenTest "TErrorPat.hs" Inplace 6 18
            , goldenTest "TQQPat.hs" Inplace 6 3
            , goldenTest "TQQPat.hs" Inplace 6 11
            , goldenTest "TQQPatError.hs" Inplace 6 3
            , goldenTest "TQQPatError.hs" Inplace 6 11
            ]
        , goldenTest "TSimpleType.hs" Inplace 5 12
        , goldenTest "TSimpleType.hs" Inplace 5 22
        , goldenTest "TTypeTypeError.hs" Inplace 7 12
        , goldenTest "TTypeTypeError.hs" Inplace 7 52
        , goldenTest "TQQType.hs" Inplace 8 19
        , goldenTest "TQQType.hs" Inplace 8 28
        , goldenTest "TQQTypeTypeError.hs" Inplace 8 19
        , goldenTest "TQQTypeTypeError.hs" Inplace 8 28
        , goldenTest "TSimpleDecl.hs" Inplace 8 1
        , goldenTest "TQQDecl.hs" Inplace 5 1
        , goldenTestWithEdit "TTypeKindError.hs" Inplace 7 9
        , goldenTestWithEdit "TDeclKindError.hs" Inplace 8 1
        ]

goldenTest :: FilePath -> ExpandStyle -> Int -> Int -> TestTree
goldenTest input tc line col =
    testCase (input <> " (golden)") $ do
        runSessionWithServer plugin spliceTestPath $ do
            doc <- openDoc input "haskell"
            _ <- waitForDiagnostics
            actions <- getCodeActions doc $ pointRange line col
            case find ((== Just (toExpandCmdTitle tc)) . codeActionTitle) actions of
                Just (InR CodeAction {_command = Just c}) -> do
                    executeCommand c
                    _resp <- skipManyTill anyMessage (message SWorkspaceApplyEdit)
                    edited <- documentContents doc
                    let expected_name = input <.> "expected"
                    -- Write golden tests if they don't already exist
                    liftIO $
                        (doesFileExist expected_name >>=) $
                            flip unless $ do
                                T.writeFile expected_name edited
                    expected <- liftIO $ T.readFile expected_name
                    liftIO $ edited @?= expected
                _ -> liftIO $ assertFailure "No CodeAction detected"

goldenTestWithEdit :: FilePath -> ExpandStyle -> Int -> Int -> TestTree
goldenTestWithEdit input tc line col =
    testCase (input <> " (golden)") $ do
        runSessionWithServer plugin spliceTestPath $ do
            doc <- openDoc input "haskell"
            orig <- documentContents doc
            let lns = T.lines orig
                theRange =
                    Range
                        { _start = Position 0 0
                        , _end = Position (length lns + 1) 1
                        }
            liftIO $ sleep 3
            alt <- liftIO $ T.readFile (input <.> "error")
            void $ applyEdit doc $ TextEdit theRange alt
            changeDoc doc [TextDocumentContentChangeEvent (Just theRange) Nothing alt]
            void waitForDiagnostics
            actions <- getCodeActions doc $ pointRange line col
            case find ((== Just (toExpandCmdTitle tc)) . codeActionTitle) actions of
                Just (InR CodeAction {_command = Just c}) -> do
                    executeCommand c
                    _resp <- skipManyTill anyMessage (message SWorkspaceApplyEdit)
                    edited <- documentContents doc
                    let expected_name = input <.> "expected"
                    -- Write golden tests if they don't already exist
                    liftIO $
                        (doesFileExist expected_name >>=) $
                            flip unless $ do
                                T.writeFile expected_name edited
                    expected <- liftIO $ T.readFile expected_name
                    liftIO $ edited @?= expected
                _ -> liftIO $ assertFailure "No CodeAction detected"

spliceTestPath :: FilePath
spliceTestPath = "test" </> "testdata"

pointRange :: Int -> Int -> Range
pointRange
    (subtract 1 -> line)
    (subtract 1 -> col) =
        Range (Position line col) (Position line $ col + 1)

-- | Get the title of a code action.
codeActionTitle :: (Command |? CodeAction) -> Maybe Text
codeActionTitle InL{}                               = Nothing
codeActionTitle (InR(CodeAction title _ _ _ _ _ _ _)) = Just title

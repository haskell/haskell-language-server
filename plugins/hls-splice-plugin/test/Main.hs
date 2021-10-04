{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module Main
  ( main
  ) where

import           Control.Monad           (void)
import           Data.List               (find)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import qualified Ide.Plugin.Splice       as Splice
import           Ide.Plugin.Splice.Types
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

splicePlugin :: PluginDescriptor IdeState
splicePlugin = Splice.descriptor "splice"

tests :: TestTree
tests = testGroup "splice"
  [ goldenTest "TSimpleExp" Inplace 6 15
  , goldenTest "TSimpleExp" Inplace 6 24
  , goldenTest "TTypeAppExp" Inplace 7 5
  , goldenTest "TErrorExp" Inplace 6 15
  , goldenTest "TErrorExp" Inplace 6 51
  , goldenTest "TQQExp" Inplace 6 17
  , goldenTest "TQQExp" Inplace 6 25
  , goldenTest "TQQExpError" Inplace 6 13
  , goldenTest "TQQExpError" Inplace 6 22
  , testGroup "Pattern Splices"
      [ goldenTest "TSimplePat" Inplace 6 3
      , goldenTest "TSimplePat" Inplace 6 22
      , goldenTest "TSimplePat" Inplace 6 3
      , goldenTest "TSimplePat" Inplace 6 22
      , goldenTest "TErrorPat" Inplace 6 3
      , goldenTest "TErrorPat" Inplace 6 18
      , goldenTest "TQQPat" Inplace 6 3
      , goldenTest "TQQPat" Inplace 6 11
      , goldenTest "TQQPatError" Inplace 6 3
      , goldenTest "TQQPatError" Inplace 6 11
      ]
  , goldenTest "TSimpleType" Inplace 5 12
  , goldenTest "TSimpleType" Inplace 5 22
  , goldenTest "TTypeTypeError" Inplace 7 12
  , goldenTest "TTypeTypeError" Inplace 7 52
  , goldenTest "TQQType" Inplace 8 19
  , goldenTest "TQQType" Inplace 8 28
  , goldenTest "TQQTypeTypeError" Inplace 8 19
  , goldenTest "TQQTypeTypeError" Inplace 8 28
  , goldenTest "TSimpleDecl" Inplace 8 1
  , goldenTest "TQQDecl" Inplace 5 1
  , goldenTestWithEdit "TTypeKindError" Inplace 7 9
  , goldenTestWithEdit "TDeclKindError" Inplace 8 1
  ]

goldenTest :: FilePath -> ExpandStyle -> Int -> Int -> TestTree
goldenTest fp tc line col =
  goldenWithHaskellDoc splicePlugin (fp <> " (golden)") testDataDir fp "expected" "hs" $ \doc -> do
    _ <- waitForDiagnostics
    -- wait for the entire build to finish, so that code actions that
    -- use stale data will get uptodate stuff
    void waitForBuildQueue
    actions <- getCodeActions doc $ pointRange line col
    case find ((== Just (toExpandCmdTitle tc)) . codeActionTitle) actions of
      Just (InR CodeAction {_command = Just c}) -> do
        executeCommand c
        void $ skipManyTill anyMessage (message SWorkspaceApplyEdit)
      _ -> liftIO $ assertFailure "No CodeAction detected"

goldenTestWithEdit :: FilePath -> ExpandStyle -> Int -> Int -> TestTree
goldenTestWithEdit fp tc line col =
  goldenWithHaskellDoc splicePlugin (fp <> " (golden)") testDataDir fp "expected" "hs" $ \doc -> do
     orig <- documentContents doc
     let
       lns = T.lines orig
       theRange =
         Range
         { _start = Position 0 0
         , _end = Position (length lns + 1) 1
         }
     waitForProgressDone -- cradle
     waitForProgressDone
     alt <- liftIO $ T.readFile (fp <.> "error.hs")
     void $ applyEdit doc $ TextEdit theRange alt
     changeDoc doc [TextDocumentContentChangeEvent (Just theRange) Nothing alt]
     void waitForDiagnostics
     -- wait for the entire build to finish
     void waitForBuildQueue
     actions <- getCodeActions doc $ pointRange line col
     case find ((== Just (toExpandCmdTitle tc)) . codeActionTitle) actions of
       Just (InR CodeAction {_command = Just c}) -> do
         executeCommand c
         void $ skipManyTill anyMessage (message SWorkspaceApplyEdit)
       _ -> liftIO $ assertFailure "No CodeAction detected"

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

pointRange :: Int -> Int -> Range
pointRange (subtract 1 -> line) (subtract 1 -> col) =
  Range (Position line col) (Position line $ col + 1)

-- | Get the title of a code action.
codeActionTitle :: (Command |? CodeAction) -> Maybe Text
codeActionTitle InL {}                    = Nothing
codeActionTitle (InR CodeAction {_title}) = Just _title

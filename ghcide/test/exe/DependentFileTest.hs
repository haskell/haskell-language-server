
{-# LANGUAGE GADTs #-}

module DependentFileTest (tests) where

import           Config
import           Control.Monad.IO.Class         (liftIO)
import qualified Data.Text                      as T
import           Development.IDE.Test           (expectDiagnostics)
import           Development.IDE.Types.Location
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types    hiding
                                                (SemanticTokenAbsolute (..),
                                                 SemanticTokenRelative (..),
                                                 SemanticTokensEdit (..),
                                                 mkRange)
import           Language.LSP.Test
import           System.Directory               (setCurrentDirectory)
import           System.FilePath                ((</>))
import           Test.Hls.FileSystem            (FileSystem, toAbsFp)
import           Test.Tasty
import Test.Hls


tests :: TestTree
tests = testGroup "addDependentFile"
    [testGroup "file-changed" [testCase "test" $ runSessionWithTestConfig (mkTestConfig "" dummyPlugin) {testShiftRoot=True, testFileTree=Just (mkIdeTestFs [])} test]
    ]
    where
      test :: FilePath -> Session ()
      test dir = do
        -- If the file contains B then no type error
        -- otherwise type error
        let depFilePath = "dep-file.txt"
        liftIO $ writeFile depFilePath "A"
        let fooContent = T.unlines
              [ "{-# LANGUAGE TemplateHaskell #-}"
              , "module Foo where"
              , "import Language.Haskell.TH.Syntax"
              , "foo :: Int"
              , "foo = 1 + $(do"
              , "               qAddDependentFile \"" <> T.pack depFilePath <> "\""
              , "               f <- qRunIO (readFile \"" <> T.pack depFilePath <> "\")"
              , "               if f == \"B\" then [| 1 |] else lift f)"
              ]
        let bazContent = T.unlines ["module Baz where", "import Foo ()"]
        _fooDoc <- createDoc "Foo.hs" "haskell" fooContent
        doc <- createDoc "Baz.hs" "haskell" bazContent
        expectDiagnostics
            [("Foo.hs", [(DiagnosticSeverity_Error, (4,11), "Couldn't match type")])]
        -- Now modify the dependent file
        liftIO $ writeFile depFilePath "B"
        sendNotification SMethod_WorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams
            [FileEvent (filePathToUri depFilePath) FileChangeType_Changed ]

        -- Modifying Baz will now trigger Foo to be rebuilt as well
        let change = TextDocumentContentChangeEvent $ InL TextDocumentContentChangePartial
                { _range = Range (Position 2 0) (Position 2 6)
                , _rangeLength = Nothing
                , _text = "f = ()"
                }
        changeDoc doc [change]
        expectDiagnostics [("Foo.hs", [])]

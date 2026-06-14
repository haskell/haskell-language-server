
{-# LANGUAGE GADTs #-}

module DependentFileTest (tests) where

import           Config
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
import           System.FilePath                ((</>))
import           Test.Hls
import           Test.Hls.FileSystem


tests :: TestTree
tests = testGroup "addDependentFile"
    [testGroup "file-changed" [testCase "test" $ runSessionWithTestConfig def
        { testCwdHandling = NoCwdShift
        , testDirLocation = Right (mkIdeTestFs [])
        , testPluginDescriptor = dummyPlugin
        } test]
    ]
    where
      test :: FilePath -> Session ()
      test sessionDir = do
        -- If the file contains B then no type error
        -- otherwise type error
        -- Absolute path so the splice's qRunIO/readFile and the watched-file
        -- notification resolve identically regardless of the process CWD.
        let depFilePath = sessionDir </> "dep-file.txt"
        -- show gives a properly escaped Haskell string literal, so a Windows
        -- path's backslashes survive the splice into Foo's source.
        let depFileLit = T.pack (show depFilePath)
        liftIO $ atomicFileWriteString depFilePath "A"
        let fooContent = T.unlines
              [ "{-# LANGUAGE TemplateHaskell #-}"
              , "module Foo where"
              , "import Language.Haskell.TH.Syntax"
              , "foo :: Int"
              , "foo = 1 + $(do"
              , "               qAddDependentFile " <> depFileLit
              , "               f <- qRunIO (readFile " <> depFileLit <> ")"
              , "               if f == \"B\" then [| 1 |] else lift f)"
              ]
        let bazContent = T.unlines ["module Baz where", "import Foo ()"]
        _fooDoc <- createDoc "Foo.hs" "haskell" fooContent
        doc <- createDoc "Baz.hs" "haskell" bazContent
        expectDiagnostics
            [("Foo.hs", [(DiagnosticSeverity_Error, (4,11), "Couldn't match type", Just "GHC-83865")])]
        -- Now modify the dependent file
        liftIO $ atomicFileWriteString depFilePath "B"
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

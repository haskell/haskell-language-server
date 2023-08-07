
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE OverloadedLabels #-}

module DependentFileTest (tests) where

import           Control.Monad.IO.Class         (liftIO)
import           Data.Row
import qualified Data.Text                      as T
import           Development.IDE.GHC.Compat     (GhcVersion (..), ghcVersion)
import           Development.IDE.Test           (expectDiagnostics)
import           Development.IDE.Types.Location
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types    hiding
                                                (SemanticTokenAbsolute (..),
                                                 SemanticTokenRelative (..),
                                                 SemanticTokensEdit (..),
                                                 mkRange)
import           Language.LSP.Test
import           System.FilePath
import           Test.Tasty
import           TestUtils

tests :: TestTree
tests = testGroup "addDependentFile"
    [testGroup "file-changed" [testSession' "test" test]
    ]
    where
      test dir = do
        -- If the file contains B then no type error
        -- otherwise type error
        let depFilePath = dir </> "dep-file.txt"
        liftIO $ writeFile depFilePath "A"
        let fooContent = T.unlines
              [ "{-# LANGUAGE TemplateHaskell #-}"
              , "module Foo where"
              , "import Language.Haskell.TH.Syntax"
              , "foo :: Int"
              , "foo = 1 + $(do"
              , "               qAddDependentFile \"dep-file.txt\""
              , "               f <- qRunIO (readFile \"dep-file.txt\")"
              , "               if f == \"B\" then [| 1 |] else lift f)"
              ]
        let bazContent = T.unlines ["module Baz where", "import Foo ()"]
        _ <- createDoc "Foo.hs" "haskell" fooContent
        doc <- createDoc "Baz.hs" "haskell" bazContent
        expectDiagnostics $
            if ghcVersion >= GHC90
                -- String vs [Char] causes this change in error message
                then [("Foo.hs", [(DiagnosticSeverity_Error, if ghcVersion >= GHC92 then (4,11) else (4, 6), "Couldn't match type")])]
                else [("Foo.hs", [(DiagnosticSeverity_Error, (4, 6), "Couldn't match expected type")])]
        -- Now modify the dependent file
        liftIO $ writeFile depFilePath "B"
        sendNotification SMethod_WorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams $
            [FileEvent (filePathToUri "dep-file.txt") FileChangeType_Changed ]

        -- Modifying Baz will now trigger Foo to be rebuilt as well
        let change = TextDocumentContentChangeEvent $ InL $ #range .== Range (Position 2 0) (Position 2 6)
                                                         .+ #rangeLength .== Nothing
                                                         .+ #text .== "f = ()"
        changeDoc doc [change]
        expectDiagnostics [("Foo.hs", [])]

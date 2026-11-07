{-# LANGUAGE OverloadedStrings #-}

module OpenCloseTest (tests) where

import           Config                          (Expect (..), mkIdeTestFs,
                                                  testWithDummyPlugin,
                                                  testWithDummyPluginEmpty)
import           Control.Applicative.Combinators
import           Control.Monad
import qualified Data.Text                       as T
import           Hover                           (checkHover)
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Test
import           Test.Hls                        (waitForAction,
                                                  waitForProgressBegin,
                                                  waitForProgressDone)
import qualified Test.Hls.FileSystem             as FS
import           Test.Tasty

tests :: TestTree
tests = testGroup "open close"
    [ testWithDummyPluginEmpty "progress on close" $ do
        doc <- createDoc "Testing.hs" "haskell" ""
        void (skipManyTill anyMessage $ message SMethod_WindowWorkDoneProgressCreate)
        waitForProgressBegin
        closeDoc doc
        waitForProgressDone
    -- https://github.com/haskell/haskell-language-server/issues/5163
    , testGroup "reopen keeps positions"
        [ reopenTest "at a lower version" $ \doc -> do
            openAt doc 1
            insertLinesAtTop doc 2
            closeDoc doc
            openAt doc 1
        , reopenTest "close and open at the same version" $ \doc -> do
            openAt doc 0
            insertLinesAtTop doc 1
            closeDoc doc
            openAt doc 0
        ]
    ]

source :: T.Text
source = "module Foo where\nx :: Int\nx = 1\n"

reopenTest :: String -> (TextDocumentIdentifier -> Session ()) -> TestTree
reopenTest name reopen =
    testWithDummyPlugin name (mkIdeTestFs [FS.directCradle ["Foo.hs"], FS.file "Foo.hs" (FS.text source)]) $ do
        doc <- TextDocumentIdentifier <$> getDocUri "Foo.hs"
        reopen doc
        _ <- waitForAction "gethieast" doc
        hover <- getHover doc (Position 2 0)
        checkHover hover [ExpectHoverText ["x :: Int"]]

openAt :: TextDocumentIdentifier -> Int32 -> Session ()
openAt (TextDocumentIdentifier uri) ver =
    sendNotification SMethod_TextDocumentDidOpen $ DidOpenTextDocumentParams $
        TextDocumentItem uri "haskell" ver source

insertLinesAtTop :: TextDocumentIdentifier -> Int32 -> Session ()
insertLinesAtTop (TextDocumentIdentifier uri) ver =
    sendNotification SMethod_TextDocumentDidChange $ DidChangeTextDocumentParams
        (VersionedTextDocumentIdentifier uri ver)
        [TextDocumentContentChangeEvent $ InL $ TextDocumentContentChangePartial
            (Range (Position 0 0) (Position 0 0)) Nothing "\n\n\n"]

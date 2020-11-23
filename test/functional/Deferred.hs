{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Deferred(tests) where

import Control.Applicative.Combinators
import Control.Monad.IO.Class
import Control.Lens hiding (List)
-- import Control.Monad
-- import Data.Maybe
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Lens hiding (id, message)
-- import qualified Language.Haskell.LSP.Types.Lens as LSP
import Test.Hls.Util
import Test.Tasty
import Test.Tasty.ExpectedFailure (ignoreTestBecause)
import Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "deferred responses" [

    --TODO: DOes not compile
    -- testCase "do not affect hover requests" $ runSession hlsCommand fullCaps "test/testdata" $ do
    --   doc <- openDoc "FuncTest.hs" "haskell"

    --   id1 <- sendRequest TextDocumentHover (TextDocumentPositionParams doc (Position 4 2) Nothing)

    --   skipMany anyNotification
    --   hoverRsp <- message :: Session HoverResponse
    --   liftIO $ hoverRsp ^? result . _Just . _Just . contents @?= Nothing
    --   liftIO $ hoverRsp ^. LSP.id @?= responseId id1

    --   id2 <- sendRequest TextDocumentDocumentSymbol (DocumentSymbolParams doc Nothing)
    --   symbolsRsp <- skipManyTill anyNotification message :: Session DocumentSymbolsResponse
    --   liftIO $ symbolsRsp ^. LSP.id @?= responseId id2

    --   id3 <- sendRequest TextDocumentHover (TextDocumentPositionParams doc (Position 4 2) Nothing)
    --   hoverRsp2 <- skipManyTill anyNotification message :: Session HoverResponse
    --   liftIO $ hoverRsp2 ^. LSP.id @?= responseId id3

    --   let contents2 = hoverRsp2 ^? result . _Just . _Just . contents
    --   liftIO $ contents2 `shouldNotSatisfy` null

    --   -- Now that we have cache the following request should be instant
    --   let highlightParams = TextDocumentPositionParams doc (Position 7 0) Nothing
    --   highlightRsp <- request TextDocumentDocumentHighlight highlightParams
    --   let (Just (List locations)) = highlightRsp ^. result
    --   liftIO $ locations @?= [ DocumentHighlight
    --                  { _range = Range
    --                    { _start = Position {_line = 7, _character = 0}
    --                    , _end   = Position {_line = 7, _character = 2}
    --                    }
    --                  , _kind  = Just HkWrite
    --                  }
    --                , DocumentHighlight
    --                  { _range = Range
    --                    { _start = Position {_line = 7, _character = 0}
    --                    , _end   = Position {_line = 7, _character = 2}
    --                    }
    --                  , _kind  = Just HkWrite
    --                  }
    --                , DocumentHighlight
    --                  { _range = Range
    --                    { _start = Position {_line = 5, _character = 6}
    --                    , _end   = Position {_line = 5, _character = 8}
    --                    }
    --                  , _kind  = Just HkRead
    --                  }
    --                , DocumentHighlight
    --                  { _range = Range
    --                    { _start = Position {_line = 7, _character = 0}
    --                    , _end   = Position {_line = 7, _character = 2}
    --                    }
    --                  , _kind  = Just HkWrite
    --                  }
    --                , DocumentHighlight
    --                  { _range = Range
    --                    { _start = Position {_line = 7, _character = 0}
    --                    , _end   = Position {_line = 7, _character = 2}
    --                    }
    --                  , _kind  = Just HkWrite
    --                  }
    --                , DocumentHighlight
    --                  { _range = Range
    --                    { _start = Position {_line = 5, _character = 6}
    --                    , _end   = Position {_line = 5, _character = 8}
    --                    }
    --                  , _kind  = Just HkRead
    --                  }
    --                ]

     testCase "instantly respond to failed modules with no cache" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "FuncTestFail.hs" "haskell"
        defs <- getDefinitions doc (Position 1 11)
        liftIO $ defs @?= []

    -- TODO: the benefits of caching parsed modules is doubted.
    -- TODO: add issue link
    -- , testCase "respond to untypecheckable modules with parsed module cache" $
    --   runSession hlsCommand fullCaps "test/testdata" $ do
    --     doc <- openDoc "FuncTestFail.hs" "haskell"
    --     (Left (sym:_)) <- getDocumentSymbols doc
    --     liftIO $ sym ^. name @?= "main"

    -- TODO does not compile
    -- , testCase "returns hints as diagnostics" $ runSession hlsCommand fullCaps "test/testdata" $ do
    --     _ <- openDoc "FuncTest.hs" "haskell"

    --     cwd <- liftIO getCurrentDirectory
    --     let testUri = filePathToUri $ cwd </> "test/testdata/FuncTest.hs"

    --     diags <- skipManyTill loggingNotification publishDiagnosticsNotification
    --     liftIO $ diags ^? params @?= (Just $ PublishDiagnosticsParams
    --                 { _uri         = testUri
    --                 , _diagnostics = List
    --                     [ Diagnostic
    --                         (Range (Position 9 6) (Position 10 18))
    --                         (Just DsInfo)
    --                         (Just (StringValue "Redundant do"))
    --                         (Just "hlint")
    --                         "Redundant do\nFound:\n  do putStrLn \"hello\"\nWhy not:\n  putStrLn \"hello\"\n"
    --                         Nothing
    --                     ]
    --                 }
    --             )
        -- let args' = H.fromList [("pos", toJSON (Position 7 0)), ("file", toJSON testUri)]
        --     args = List [Object args']
        --
        -- executeRsp <- request WorkspaceExecuteCommand (ExecuteCommandParams "hare:demote" (Just args) Nothing)
        -- liftIO $ executeRsp ^. result @?= Just (Object H.empty)

        -- editReq <- message :: Session ApplyWorkspaceEditRequest
        -- let expectedTextEdits = List [TextEdit (Range (Position 6 0) (Position 7 6)) "  where\n    bb = 5"]
        --     expectedTextDocEdits = List [TextDocumentEdit (VersionedTextDocumentIdentifier testUri (Just 0)) expectedTextEdits]
        -- liftIO $ editReq ^. params . edit @?= WorkspaceEdit
        --       Nothing
        --       (Just expectedTextDocEdits)
    -- , multiServerTests
    , multiMainTests
    ]

--TODO: Does not compile
-- multiServerTests :: TestTree
-- multiServerTests = testGroup "multi-server setup" [
--     testCase "doesn't have clashing commands on two servers" $ do
--         let getCommands = runSession hlsCommand fullCaps "test/testdata" $ do
--                 rsp <- initializeResponse
--                 let uuids = rsp ^? result . _Just . capabilities . executeCommandProvider . _Just . commands
--                 return $ fromJust uuids
--         List uuids1 <- getCommands
--         List uuids2 <- getCommands
--         liftIO $ forM_ (zip uuids1 uuids2) (uncurry shouldNotBe)
--     ]

multiMainTests :: TestTree
multiMainTests = testGroup "multiple main modules" [
    ignoreTestBecause "Broken: Unexpected ConduitParser.empty" $
    testCase "Can load one file at a time, when more than one Main module exists"
        $ runSession hlsCommand fullCaps "test/testdata" $ do
            _doc <- openDoc "ApplyRefact2.hs" "haskell"
            _diagsRspHlint <- skipManyTill anyNotification message :: Session PublishDiagnosticsNotification
            diagsRspGhc    <- skipManyTill anyNotification message :: Session PublishDiagnosticsNotification
            let (List diags) = diagsRspGhc ^. params . diagnostics

            liftIO $ length diags @?= 2

            _doc2 <- openDoc "HaReRename.hs" "haskell"
            _diagsRspHlint2 <- skipManyTill anyNotification message :: Session PublishDiagnosticsNotification
            -- errMsg <- skipManyTill anyNotification notification :: Session ShowMessageNotification
            diagsRsp2 <- skipManyTill anyNotification message :: Session PublishDiagnosticsNotification
            let (List diags2) = diagsRsp2 ^. params . diagnostics

            liftIO $ show diags2 @?= "[]"
    ]

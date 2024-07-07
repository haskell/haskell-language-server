{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main ( main ) where

import           Data.Either               (rights)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Ide.Plugin.ExplicitFields as ExplicitFields
import           System.FilePath           ((<.>), (</>))
import           Test.Hls

main :: IO ()
main = defaultTestRunner test

plugin :: PluginTestDescriptor ExplicitFields.Log
plugin = mkPluginTestDescriptor ExplicitFields.descriptor "explicit-fields"

test :: TestTree
test = testGroup "explicit-fields"
  [ testGroup "code actions"
    [ mkTest "WildcardOnly" "WildcardOnly" 12 10 12 20
    , mkTest "Unused" "Unused" 12 10 12 20
    , mkTest "Unused2" "Unused2" 12 10 12 20
    , mkTest "WithPun" "WithPun" 13 10 13 25
    , mkTest "WithExplicitBind" "WithExplicitBind" 12 10 12 32
    , mkTest "Mixed" "Mixed" 14 10 14 37
    , mkTest "Construction" "Construction" 16 5 16 15
    , mkTest "HsExpanded1" "HsExpanded1" 17 10 17 20
    , mkTest "HsExpanded2" "HsExpanded2" 23 10 23 22
    , mkTestNoAction "ExplicitBinds" "ExplicitBinds" 11 10 11 52
    , mkTestNoAction "Puns" "Puns" 12 10 12 31
    , mkTestNoAction "Infix" "Infix" 11 11 11 31
    , mkTestNoAction "Prefix" "Prefix" 10 11 10 28
    ]
  , testGroup "inlay hints"
    [ mkInlayHintsTest "Construction" 16 $ (@=?)
        [defInlayHint { _position = Position 16 14
                      , _label = InR [ mkLabelPart "foo", commaPart
                                     , mkLabelPart "bar", commaPart
                                     , mkLabelPart "baz"
                                     ]
                      , _textEdits = Just [ mkLineTextEdit "MyRec {foo, bar, baz}" 16 5 15
                                          , mkPragmaTextEdit 2
                                          ]
                      , _tooltip = Just $ InL "Expand record wildcard (needs extension: NamedFieldPuns)"
                      }]
    , mkInlayHintsTest "HsExpanded1" 17 $ (@=?)
        [defInlayHint { _position = Position 17 19
                      , _label = InR [ mkLabelPart "foo" ]
                      , _textEdits = Just [ mkLineTextEdit "MyRec {foo}" 17 10 20 ]
                      , _tooltip = Just $ InL "Expand record wildcard"
                      }]
    , mkInlayHintsTest "HsExpanded2" 23 $ (@=?)
        [defInlayHint { _position = Position 23 21
                      , _label = InR [ mkLabelPart "bar" ]
                      , _textEdits = Just [ mkLineTextEdit "YourRec {bar}" 23 10 22 ]
                      , _tooltip = Just $ InL "Expand record wildcard"
                      }]
    , mkInlayHintsTest "Mixed" 14 $ (@=?)
        [defInlayHint { _position = Position 14 36
                      , _label = InR [ mkLabelPart "baz", commaPart
                                     , mkLabelPart "quux"
                                     ]
                      , _textEdits = Just [ mkLineTextEdit "MyRec {foo, bar = bar', baz}" 14 10 37 ]
                      , _tooltip = Just $ InL "Expand record wildcard"
                      }]
    , mkInlayHintsTest "Unused" 12 $ (@=?)
        [defInlayHint { _position = Position 12 19
                      , _label = InR [ mkLabelPart "foo", commaPart
                                     , mkLabelPart "bar", commaPart
                                     , mkLabelPart "baz"
                                     ]
                      , _textEdits = Just [ mkLineTextEdit "MyRec {foo, bar}" 12 10 20
                                          , mkPragmaTextEdit 2
                                          ]
                      , _tooltip = Just $ InL "Expand record wildcard (needs extension: NamedFieldPuns)"
                      }]
    , mkInlayHintsTest "Unused2" 12 $ (@=?)
        [defInlayHint { _position = Position 12 19
                      , _label = InR [ mkLabelPart "foo", commaPart
                                     , mkLabelPart "bar", commaPart
                                     , mkLabelPart "baz"
                                     ]
                      , _textEdits = Just [ mkLineTextEdit "MyRec {foo, bar}" 12 10 20
                                          , mkPragmaTextEdit 2
                                          ]
                      , _tooltip = Just $ InL "Expand record wildcard (needs extension: NamedFieldPuns)"
                      }]
    , mkInlayHintsTest "Unused2" 12 $ (@=?)
        [defInlayHint { _position = Position 12 19
                      , _label = InR [ mkLabelPart "foo", commaPart
                                     , mkLabelPart "bar", commaPart
                                     , mkLabelPart "baz"
                                     ]
                      , _textEdits = Just [ mkLineTextEdit "MyRec {foo, bar}" 12 10 20
                                          , mkPragmaTextEdit 2
                                          ]
                      , _tooltip = Just $ InL "Expand record wildcard (needs extension: NamedFieldPuns)"
                      }]
    , mkInlayHintsTest "WildcardOnly" 12 $ (@=?)
        [defInlayHint { _position = Position 12 19
                      , _label = InR [ mkLabelPart "foo", commaPart
                                     , mkLabelPart "bar", commaPart
                                     , mkLabelPart "baz"
                                     ]
                      , _textEdits = Just [ mkLineTextEdit "MyRec {foo, bar, baz}" 12 10 20
                                          , mkPragmaTextEdit 2
                                          ]
                      , _tooltip = Just $ InL "Expand record wildcard (needs extension: NamedFieldPuns)"
                      }]
    , mkInlayHintsTest "WithExplicitBind" 12 $ (@=?)
        [defInlayHint { _position = Position 12 31
                      , _label = InR [ mkLabelPart "bar", commaPart
                                     , mkLabelPart "baz"
                                     ]
                      , _textEdits = Just [ mkLineTextEdit "MyRec {foo = foo', bar, baz}" 12 10 32
                                          , mkPragmaTextEdit 2
                                          ]
                      , _tooltip = Just $ InL "Expand record wildcard (needs extension: NamedFieldPuns)"
                      }]
    , mkInlayHintsTest "WithPun" 13 $ (@=?)
        [defInlayHint { _position = Position 13 24
                      , _label = InR [ mkLabelPart "bar", commaPart
                                     , mkLabelPart "baz"
                                     ]
                      , _textEdits = Just [ mkLineTextEdit "MyRec {foo, bar, baz}" 13 10 25 ]
                      , _tooltip = Just $ InL "Expand record wildcard"
                      }]
    ]
  ]

mkInlayHintsTest :: FilePath -> UInt -> ([InlayHint] -> Assertion) -> TestTree
mkInlayHintsTest fp line assert =
  testCase fp $
    runSessionWithServer def plugin testDataDir $ do
      doc <- openDoc (fp ++ ".hs") "haskell"
      inlayHints <- getInlayHints doc (lineRange line)
      liftIO $ assert inlayHints
  where
    lineRange line = Range (Position line 0) (Position line 1000)

mkTestNoAction :: TestName -> FilePath -> UInt -> UInt -> UInt -> UInt -> TestTree
mkTestNoAction title fp x1 y1 x2 y2 =
  testCase title $
    runSessionWithServer def plugin (testDataDir </> "noop") $ do
      doc <- openDoc (fp <.> "hs") "haskell"
      actions <- getExplicitFieldsActions doc x1 y1 x2 y2
      liftIO $ actions @?= []

mkTestWithCount :: Int -> TestName -> FilePath -> UInt -> UInt -> UInt -> UInt -> TestTree
mkTestWithCount cnt title fp x1 y1 x2 y2 =
  goldenWithHaskellAndCaps def codeActionResolveCaps plugin title testDataDir fp "expected" "hs" $ \doc -> do
    acts@(act:_) <- getExplicitFieldsActions doc x1 y1 x2 y2
    liftIO $ length acts @?= cnt
    executeCodeAction act

mkTest :: TestName -> FilePath -> UInt -> UInt -> UInt -> UInt -> TestTree
mkTest = mkTestWithCount 1

getExplicitFieldsActions
  :: TextDocumentIdentifier
  -> UInt -> UInt -> UInt -> UInt
  -> Session [CodeAction]
getExplicitFieldsActions doc x1 y1 x2 y2 =
  findExplicitFieldsAction <$> getAndResolveCodeActions doc range
  where
    range = Range (Position x1 y1) (Position x2 y2)

findExplicitFieldsAction :: [a |? CodeAction] -> [CodeAction]
findExplicitFieldsAction = filter isExplicitFieldsCodeAction . rights . map toEither

isExplicitFieldsCodeAction :: CodeAction -> Bool
isExplicitFieldsCodeAction CodeAction {_title} =
  "Expand record wildcard" `T.isPrefixOf` _title

defInlayHint :: InlayHint
defInlayHint =
  InlayHint
  { _position     = Position 0 0
  , _label        = InR []
  , _kind         = Nothing
  , _textEdits    = Nothing
  , _tooltip      = Nothing
  , _paddingLeft  = Nothing
  , _paddingRight = Nothing
  , _data_        = Nothing
  }

mkLabelPart :: Text -> InlayHintLabelPart
mkLabelPart value =
  InlayHintLabelPart
  { _location = Nothing
  , _value    = value
  , _tooltip  = Nothing
  , _command  = Nothing
  }

commaPart :: InlayHintLabelPart
commaPart =
  InlayHintLabelPart
  { _location = Nothing
  , _value    = ", "
  , _tooltip  = Nothing
  , _command  = Nothing
  }

mkLineTextEdit :: Text -> UInt -> UInt -> UInt -> TextEdit
mkLineTextEdit newText line x y =
  TextEdit
  { _range = Range (Position line x) (Position line y)
  , _newText = newText
  }

mkPragmaTextEdit :: UInt -> TextEdit
mkPragmaTextEdit line =
  TextEdit
  { _range = Range (Position line 0) (Position line 0)
  , _newText = "{-# LANGUAGE NamedFieldPuns #-}\n"
  }

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-explicit-record-fields-plugin" </> "test" </> "testdata"

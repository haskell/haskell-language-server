{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main ( main ) where

import           Data.Either               (rights)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Development.IDE           (filePathToUri',
                                            toNormalizedFilePath')
import           Development.IDE.Test      (canonicalizeUri)
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
    [ mkExpansionTest "WildcardOnly" "WildcardOnly" 12 10 12 20
    , mkExpansionTest "Unused" "Unused" 12 10 12 20
    , mkExpansionTest "Unused2" "Unused2" 12 10 12 20
    , mkExpansionTest "WithPun" "WithPun" 13 10 13 25
    , mkExpansionTest "WithExplicitBind" "WithExplicitBind" 12 10 12 32
    , mkExpansionTest "Mixed" "Mixed" 14 10 14 37
    , mkExpansionTest "Construction" "Construction" 16 5 16 15
    , mkConversionTest "PositionalConstruction" "PositionalConstruction" 15 5 15 15
    , mkExpansionTest "HsExpanded1" "HsExpanded1" 17 10 17 20
    , mkExpansionTest "HsExpanded2" "HsExpanded2" 23 10 23 22
    , mkTestNoAction "ExplicitBinds" "ExplicitBinds" 11 10 11 52
    , mkTestNoAction "Puns" "Puns" 12 10 12 31
    , mkTestNoAction "Infix" "Infix" 11 11 11 31
    , mkTestNoAction "Prefix" "Prefix" 10 11 10 28
    , mkTestNoAction "PartiallyAppliedCon" "PartiallyAppliedCon" 7 8 7 12
    , mkConversionTest "PolymorphicRecordConstruction" "PolymorphicRecordConstruction" 15 5 15 15
    , mkConversionTest "CursorAwarePositional" "CursorPositional" 15 26 15 34
    , mkExpansionTest "CursorAwareRecords" "CursorRecords" 9 40 9 40
    ]
  , testGroup "inlay hints"
    [ mkInlayHintsTest "Construction" Nothing 16 $ \ih -> do
        let mkLabelPart' = mkLabelPartOffsetLength "Construction"
        foo <- mkLabelPart' 13 6 "foo"
        bar <- mkLabelPart' 14 6 "bar"
        baz <- mkLabelPart' 15 6 "baz"
        (@?=) ih
          [defInlayHint { _position = Position 16 14
                        , _label = InR [ foo, commaPart
                                       , bar, commaPart
                                       , baz
                                       ]
                        , _textEdits = Just [ mkLineTextEdit "MyRec {foo, bar, baz}" 16 5 15
                                            , mkPragmaTextEdit 2
                                            ]
                        , _tooltip = Just $ InL "Expand record wildcard (needs extension: NamedFieldPuns)"
                        , _paddingLeft = Just True
                        }]
    , mkInlayHintsTest "ConstructionDuplicateRecordFields" Nothing 16 $ \ih -> do
        let mkLabelPart' = mkLabelPartOffsetLength "ConstructionDuplicateRecordFields"
        foo <- mkLabelPart' 13 6 "foo"
        bar <- mkLabelPart' 14 6 "bar"
        baz <- mkLabelPart' 15 6 "baz"
        (@?=) ih
          [defInlayHint { _position = Position 16 14
                        , _label = InR [ foo, commaPart
                                       , bar, commaPart
                                       , baz
                                       ]
                        , _textEdits = Just [ mkLineTextEdit "MyRec {foo, bar, baz}" 16 5 15
                                            , mkPragmaTextEdit 3 -- Not 2 of the DuplicateRecordFields pragma
                                            ]
                        , _tooltip = Just $ InL "Expand record wildcard (needs extension: NamedFieldPuns)"
                        , _paddingLeft = Just True
                        }]

    , mkInlayHintsTest "PositionalConstruction" Nothing 15 $ \ih -> do
        let mkLabelPart' = mkLabelPartOffsetLengthSub1 "PositionalConstruction"
        foo <- mkLabelPart' 5 4 "foo="
        bar <- mkLabelPart' 6 4 "bar="
        baz <- mkLabelPart' 7 4 "baz="
        (@?=) ih
          [ defInlayHint { _position = Position 15 11
                         , _label = InR [ foo ]
                         , _textEdits = Just [ mkLineTextEdit "MyRec { foo = a, bar = b, baz = c }" 15 5 16 ]
                         , _tooltip = Just $ InL "Convert to traditional record syntax"
                         , _paddingLeft = Nothing
                         }
          , defInlayHint { _position = Position 15 13
                         , _label = InR [ bar ]
                         , _textEdits = Just [ mkLineTextEdit "MyRec { foo = a, bar = b, baz = c }" 15 5 16 ]
                         , _tooltip = Just $ InL "Convert to traditional record syntax"
                         , _paddingLeft = Nothing
                         }
          , defInlayHint { _position = Position 15 15
                         , _label = InR [ baz ]
                         , _textEdits = Just [ mkLineTextEdit "MyRec { foo = a, bar = b, baz = c }" 15 5 16 ]
                         , _tooltip = Just $ InL "Convert to traditional record syntax"
                         , _paddingLeft = Nothing
                         }
          ]
    , mkInlayHintsTest "PositionalConstructionDuplicateRecordFields" Nothing 15 $ \ih -> do
        let mkLabelPart' = mkLabelPartOffsetLengthSub1 "PositionalConstructionDuplicateRecordFields"
        foo <- mkLabelPart' 5 4 "foo="
        bar <- mkLabelPart' 6 4 "bar="
        baz <- mkLabelPart' 7 4 "baz="
        (@?=) ih
          [ defInlayHint { _position = Position 15 11
                         , _label = InR [ foo ]
                         , _textEdits = Just [ mkLineTextEdit "MyRec { foo = a, bar = b, baz = c }" 15 5 16 ]
                         , _tooltip = Just $ InL "Convert to traditional record syntax"
                         , _paddingLeft = Nothing
                         }
          , defInlayHint { _position = Position 15 13
                         , _label = InR [ bar ]
                         , _textEdits = Just [ mkLineTextEdit "MyRec { foo = a, bar = b, baz = c }" 15 5 16 ]
                         , _tooltip = Just $ InL "Convert to traditional record syntax"
                         , _paddingLeft = Nothing
                         }
          , defInlayHint { _position = Position 15 15
                         , _label = InR [ baz ]
                         , _textEdits = Just [ mkLineTextEdit "MyRec { foo = a, bar = b, baz = c }" 15 5 16 ]
                         , _tooltip = Just $ InL "Convert to traditional record syntax"
                         , _paddingLeft = Nothing
                         }
          ]
    , mkInlayHintsTest "HsExpanded1" Nothing 17 $ \ih -> do
        let mkLabelPart' = mkLabelPartOffsetLength "HsExpanded1"
        foo <- mkLabelPart' 11 4 "foo"
        (@?=) ih
          [defInlayHint { _position = Position 17 19
                        , _label = InR [ foo ]
                        , _textEdits = Just [ mkLineTextEdit "MyRec {foo}" 17 10 20 ]
                        , _tooltip = Just $ InL "Expand record wildcard"
                        , _paddingLeft = Just True
                        }]
    , mkInlayHintsTest "HsExpanded1" (Just " (positional)") 13 $ \ih -> do
        let mkLabelPart' = mkLabelPartOffsetLengthSub1 "HsExpanded1"
        foo <- mkLabelPart' 11 4 "foo="
        (@?=) ih
          [defInlayHint { _position = Position 13 21
                        , _label = InR [ foo ]
                        , _textEdits = Just [ mkLineTextEdit "MyRec { foo = 5 }" 13 15 22 ]
                        , _tooltip = Just $ InL "Convert to traditional record syntax"
                        , _paddingLeft = Nothing
                        }]
    , mkInlayHintsTest "HsExpanded1DuplicateRecordFields" (Just " (positional)") 13 $ \ih -> do
        let mkLabelPart' = mkLabelPartOffsetLengthSub1 "HsExpanded1DuplicateRecordFields"
        foo <- mkLabelPart' 11 4 "foo="
        (@?=) ih
          [defInlayHint { _position = Position 13 21
                        , _label = InR [ foo ]
                        , _textEdits = Just [ mkLineTextEdit "MyRec { foo = 5 }" 13 15 22 ]
                        , _tooltip = Just $ InL "Convert to traditional record syntax"
                        , _paddingLeft = Nothing
                        }]
    , mkInlayHintsTest "HsExpanded2" Nothing 23 $ \ih -> do
        let mkLabelPart' = mkLabelPartOffsetLength "HsExpanded2"
        bar <- mkLabelPart' 14 4 "bar"
        (@?=) ih
          [defInlayHint { _position = Position 23 21
                        , _label = InR [ bar ]
                        , _textEdits = Just [ mkLineTextEdit "YourRec {bar}" 23 10 22 ]
                        , _tooltip = Just $ InL "Expand record wildcard"
                        , _paddingLeft = Just True
                        }]
    , mkInlayHintsTest "HsExpanded2" (Just " (positional)") 16 $ \ih -> do
        let mkLabelPart' = mkLabelPartOffsetLengthSub1 "HsExpanded2"
        foo <- mkLabelPart' 11 4 "foo="
        (@?=) ih
          [defInlayHint { _position = Position 16 21
                        , _label = InR [ foo ]
                        , _textEdits = Just [ mkLineTextEdit "MyRec { foo = 5 }" 16 15 22 ]
                        , _tooltip = Just $ InL "Convert to traditional record syntax"
                        , _paddingLeft = Nothing
                        }]
    , mkInlayHintsTest "Mixed" Nothing 14 $ \ih -> do
        let mkLabelPart' = mkLabelPartOffsetLength "Mixed"
        baz <- mkLabelPart' 9 4 "baz"
        quux <- mkLabelPart' 10 4 "quux"
        (@?=) ih
          [defInlayHint { _position = Position 14 36
                        , _label = InR [ baz, commaPart
                                       , quux
                                       ]
                        , _textEdits = Just [ mkLineTextEdit "MyRec {foo, bar = bar', baz}" 14 10 37 ]
                        , _tooltip = Just $ InL "Expand record wildcard"
                        , _paddingLeft = Just True
                        }]
    , mkInlayHintsTest "Unused" Nothing 12 $ \ih -> do
        let mkLabelPart' = mkLabelPartOffsetLength "Unused"
        foo <- mkLabelPart' 6 4 "foo"
        bar <- mkLabelPart' 7 4 "bar"
        baz <- mkLabelPart' 8 4 "baz"
        (@?=) ih
          [defInlayHint { _position = Position 12 19
                        , _label = InR [ foo, commaPart
                                       , bar, commaPart
                                       , baz
                                       ]
                        , _textEdits = Just [ mkLineTextEdit "MyRec {foo, bar}" 12 10 20
                                            , mkPragmaTextEdit 2
                                            ]
                        , _tooltip = Just $ InL "Expand record wildcard (needs extension: NamedFieldPuns)"
                        , _paddingLeft = Just True
                        }]
    , mkInlayHintsTest "Unused2" Nothing 12 $ \ih -> do
        let mkLabelPart' = mkLabelPartOffsetLength "Unused2"
        foo <- mkLabelPart' 6 4 "foo"
        bar <- mkLabelPart' 7 4 "bar"
        baz <- mkLabelPart' 8 4 "baz"
        (@?=) ih
          [defInlayHint { _position = Position 12 19
                        , _label = InR [ foo, commaPart
                                       , bar, commaPart
                                       , baz
                                       ]
                        , _textEdits = Just [ mkLineTextEdit "MyRec {foo, bar}" 12 10 20
                                            , mkPragmaTextEdit 2
                                            ]
                        , _tooltip = Just $ InL "Expand record wildcard (needs extension: NamedFieldPuns)"
                        , _paddingLeft = Just True
                        }]
    , mkInlayHintsTest "WildcardOnly" Nothing 12 $ \ih -> do
        let mkLabelPart' = mkLabelPartOffsetLength "WildcardOnly"
        foo <- mkLabelPart' 6 4 "foo"
        bar <- mkLabelPart' 7 4 "bar"
        baz <- mkLabelPart' 8 4 "baz"
        (@?=) ih
          [defInlayHint { _position = Position 12 19
                        , _label = InR [ foo, commaPart
                                       , bar, commaPart
                                       , baz
                                       ]
                        , _textEdits = Just [ mkLineTextEdit "MyRec {foo, bar, baz}" 12 10 20
                                            , mkPragmaTextEdit 2
                                            ]
                        , _tooltip = Just $ InL "Expand record wildcard (needs extension: NamedFieldPuns)"
                        , _paddingLeft = Just True
                        }]
    , mkInlayHintsTest "WithExplicitBind" Nothing 12 $ \ih -> do
        let mkLabelPart' = mkLabelPartOffsetLength "WithExplicitBind"
        bar <- mkLabelPart' 7 4 "bar"
        baz <- mkLabelPart' 8 4 "baz"
        (@?=) ih
          [defInlayHint { _position = Position 12 31
                        , _label = InR [ bar, commaPart
                                       , baz
                                       ]
                        , _textEdits = Just [ mkLineTextEdit "MyRec {foo = foo', bar, baz}" 12 10 32
                                            , mkPragmaTextEdit 2
                                            ]
                        , _tooltip = Just $ InL "Expand record wildcard (needs extension: NamedFieldPuns)"
                        , _paddingLeft = Just True
                        }]
    , mkInlayHintsTest "WithPun" Nothing 13 $ \ih -> do
        let mkLabelPart' = mkLabelPartOffsetLength "WithPun"
        bar <- mkLabelPart' 8 4 "bar"
        baz <- mkLabelPart' 9 4 "baz"
        (@?=) ih
          [defInlayHint { _position = Position 13 24
                        , _label = InR [ bar, commaPart
                                       , baz
                                       ]
                        , _textEdits = Just [ mkLineTextEdit "MyRec {foo, bar, baz}" 13 10 25 ]
                        , _tooltip = Just $ InL "Expand record wildcard"
                        , _paddingLeft = Just True
                        }]
    , mkInlayHintsTest "PolymorphicRecordConstruction" Nothing 15 $ \ih -> do
        let mkLabelPart' = mkLabelPartOffsetLengthSub1 "PolymorphicRecordConstruction"
        foo <- mkLabelPart' 5 4 "foo="
        bar <- mkLabelPart' 6 4 "bar="
        baz <- mkLabelPart' 7 4 "baz="
        (@?=) ih
          [ defInlayHint { _position = Position 15 11
                         , _label = InR [ foo ]
                         , _textEdits = Just [ mkLineTextEdit "MyRec { foo = a, bar = b, baz = c }" 15 5 16 ]
                         , _tooltip = Just $ InL "Convert to traditional record syntax"
                         , _paddingLeft = Nothing
                         }
          , defInlayHint { _position = Position 15 13
                         , _label = InR [ bar ]
                         , _textEdits = Just [ mkLineTextEdit "MyRec { foo = a, bar = b, baz = c }" 15 5 16 ]
                         , _tooltip = Just $ InL "Convert to traditional record syntax"
                         , _paddingLeft = Nothing
                         }
          , defInlayHint { _position = Position 15 15
                         , _label = InR [ baz ]
                         , _textEdits = Just [ mkLineTextEdit "MyRec { foo = a, bar = b, baz = c }" 15 5 16 ]
                         , _tooltip = Just $ InL "Convert to traditional record syntax"
                         , _paddingLeft = Nothing
                         }
          ]
    , mkInlayHintsTest "CursorRecords" Nothing 9 $ \ih -> do
        let mkLabelPart' = mkLabelPartOffsetLength "CursorRecords"
        a0  <- mkLabelPart' 3 14 "a0"
        a1  <- mkLabelPart' 4 14 "a1"
        a11 <- mkLabelPart' 4 25 "a11"
        a2  <- mkLabelPart' 5 14 "a2"
        a3  <- mkLabelPart' 6 14 "a3"
        (@?=) ih
          [ defInlayHint
              { _position = Position 9 52
              , _label = InR [ a3 ]
              , _textEdits = Just [ mkLineTextEdit "L1 {l2 = L2 {l3 = L3 {l4 = L4 {..}, ..}, ..}, a3}" 9 5 53 ]
              , _tooltip = Just $ InL "Expand record wildcard"
              , _paddingLeft = Just True
              , _paddingRight = Nothing
              , _data_ = Nothing
              }
          , defInlayHint
              { _position = Position 9 47
              , _label = InR [ a2 ]
              , _textEdits = Just [ mkLineTextEdit "L2 {l3 = L3 {l4 = L4 {..}, ..}, a2}" 9 14 48 ]
              , _tooltip = Just $ InL "Expand record wildcard"
              , _paddingLeft = Just True
              , _paddingRight = Nothing
              , _data_ = Nothing
              }
          , defInlayHint
              { _position = Position 9 42
              , _label = InR [ a1 , InlayHintLabelPart ", " Nothing Nothing Nothing , a11 ]
              , _textEdits = Just [ mkLineTextEdit "L3 {l4 = L4 {..}, a1, a11}" 9 23 43 ]
              , _tooltip = Just $ InL "Expand record wildcard"
              , _paddingLeft = Just True
              , _paddingRight = Nothing
              , _data_ = Nothing
              }
          , defInlayHint
              { _position = Position 9 37
              , _label = InR [ a0 ]
              , _textEdits = Just [ mkLineTextEdit "L4 {a0}" 9 31 38 ]
              , _tooltip = Just $ InL "Expand record wildcard"
              , _paddingLeft = Just True
              , _paddingRight = Nothing
              , _data_ = Nothing
              }
          ]
    , mkInlayHintsTest "CursorPositional" Nothing 15 $ \ih -> do
        let mkLabelPart' = mkLabelPartOffsetLengthSub1 "CursorPositional"
        middle <- mkLabelPart' 2 2 "middle="
        inner  <- mkLabelPart' 6 2 "inner="
        foo    <- mkLabelPart' 10 2 "foo="
        bar    <- mkLabelPart' 11 4 "bar="
        (@?=) ih
          [ defInlayHint
              { _position = Position 15 14
              , _label = InR [ middle ]
              , _textEdits = Just [ mkLineTextEdit "RecOuter { middle = (RecMiddle (RecInner 'c' 42)) }" 15 5 43 ]
              , _tooltip = Just $ InL "Convert to traditional record syntax"
              , _paddingLeft = Nothing
              }
          , defInlayHint
              { _position = Position 15 25
              , _label = InR [ inner ]
              , _textEdits = Just [ mkLineTextEdit "RecMiddle { inner = (RecInner 'c' 42) }" 15 15 42 ]
              , _tooltip = Just $ InL "Convert to traditional record syntax"
              , _paddingLeft = Nothing
              }
          , defInlayHint
              { _position = Position 15 35
              , _label = InR [ foo ]
              , _textEdits =
                  Just [ mkLineTextEdit "RecInner { foo = 'c', bar = 42 }" 15 26 41 ]
              , _tooltip = Just $ InL "Convert to traditional record syntax"
              , _paddingLeft = Nothing
              }
          , defInlayHint
              { _position = Position 15 39
              , _label = InR [ bar ]
              , _textEdits =
                  Just [ mkLineTextEdit "RecInner { foo = 'c', bar = 42 }" 15 26 41 ]
              , _tooltip = Just $ InL "Convert to traditional record syntax"
              , _paddingLeft = Nothing
              }
          ]
    ]
  ]

mkInlayHintsTest :: FilePath -> Maybe TestName -> UInt -> ([InlayHint] -> Assertion) -> TestTree
mkInlayHintsTest fp postfix line assert =
  testCase (fp ++ concat postfix) $
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
      actions <- getActions isExplicitFieldsCodeAction doc x1 y1 x2 y2
      liftIO $ actions @?= []

mkTestWithCountAndMessage :: (CodeAction -> Bool) -> Int -> TestName -> FilePath -> UInt -> UInt -> UInt -> UInt -> TestTree
mkTestWithCountAndMessage msgPredicate cnt title fp x1 y1 x2 y2 =
  goldenWithHaskellAndCaps def codeActionResolveCaps plugin title testDataDir fp "expected" "hs" $ \doc -> do
    acts@(act:_) <- getActions msgPredicate doc x1 y1 x2 y2
    liftIO $ length acts @?= cnt
    executeCodeAction act

mkExpansionTest :: TestName -> FilePath -> UInt -> UInt -> UInt -> UInt -> TestTree
mkExpansionTest = mkTestWithCountAndMessage isRecWildcardExpansionCodeAction 1

mkConversionTest :: TestName -> FilePath -> UInt -> UInt -> UInt -> UInt -> TestTree
mkConversionTest = mkTestWithCountAndMessage isRecLiteralConversionCodeAction 1

getActions
  :: (CodeAction -> Bool) -> TextDocumentIdentifier
  -> UInt -> UInt -> UInt -> UInt
  -> Session [CodeAction]
getActions p doc x1 y1 x2 y2 =
  findAction p <$> getAndResolveCodeActions doc range
  where
    range = Range (Position x1 y1) (Position x2 y2)

findAction :: (CodeAction -> Bool) -> [a |? CodeAction] -> [CodeAction]
findAction p = filter p . rights . map toEither

isExplicitFieldsCodeAction :: CodeAction -> Bool
isExplicitFieldsCodeAction act =
  isRecWildcardExpansionCodeAction act
    || isRecLiteralConversionCodeAction act

isRecWildcardExpansionCodeAction :: CodeAction -> Bool
isRecWildcardExpansionCodeAction CodeAction {_title} =
  "Expand record wildcard" `T.isPrefixOf` _title

isRecLiteralConversionCodeAction :: CodeAction -> Bool
isRecLiteralConversionCodeAction CodeAction {_title} =
  "Convert to traditional record syntax" `T.isPrefixOf` _title

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

mkLabelPart :: (Text -> UInt) -> FilePath -> UInt -> UInt -> Text -> IO InlayHintLabelPart
mkLabelPart offset fp line start value = do
  uri' <- uri
  pure $ InlayHintLabelPart { _location = Just (location uri' line start)
                            , _value    = value
                            , _tooltip  = Nothing
                            , _command  = Nothing
                            }
  where
    toUri = fromNormalizedUri . filePathToUri' . toNormalizedFilePath'
    uri = canonicalizeUri $ toUri (testDataDir </> (fp ++ ".hs"))
    location uri line char = Location uri (Range (Position line char) (Position line (char + offset value)))

mkLabelPartOffsetLength :: FilePath -> UInt -> UInt -> Text -> IO InlayHintLabelPart
mkLabelPartOffsetLength = mkLabelPart (fromIntegral . T.length)

mkLabelPartOffsetLengthSub1 :: FilePath -> UInt -> UInt -> Text -> IO InlayHintLabelPart
mkLabelPartOffsetLengthSub1 = mkLabelPart (fromIntegral . subtract 1 . T.length)

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

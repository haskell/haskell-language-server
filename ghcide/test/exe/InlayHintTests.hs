{-# LANGUAGE ExplicitNamespaces #-}

module InlayHintTests (tests) where

import           Config                      (mkIdeTestFs, testWithDummyPlugin,
                                              testWithDummyPluginEmpty)
import           Control.Monad               (void)
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import qualified Data.Aeson                  as A
import           Data.Maybe                  (mapMaybe)
import qualified Data.Text                   as T
import           Language.LSP.Protocol.Types (InlayHint (..),
                                              Position (Position),
                                              Range (Range, _end, _start),
                                              TextDocumentIdentifier (TextDocumentIdentifier),
                                              TextEdit (TextEdit, _newText, _range),
                                              UInt,
                                              VersionedTextDocumentIdentifier (_uri),
                                              type (|?) (..))
import           Language.LSP.Test           (applyEdit, createDoc,
                                              documentContents, getInlayHints,
                                              openDoc, setConfigSection)
import           Test.Hls                    (Assertion, Session, expectFail,
                                              waitForTypecheck)
import           Test.Hls.FileSystem         (copyDir)
import           Test.Tasty                  (TestTree, testGroup)
import           Test.Tasty.HUnit            ((@=?), (@?=))

tests :: TestTree
tests = testGroup "inlay hints"
  [ whereInlayHintsTests
  ]

whereInlayHintsTests :: TestTree
whereInlayHintsTests = testGroup "add signature for where clauses"
  [ testWithDummyPluginEmpty "No where inlay hints if disabled" $ do
      let content = T.unlines
            [ "module Sigs where"
            , "f :: b"
            , "f = undefined"
            , "  where"
            , "    g = True"
            ]
          range = Range { _start = Position 4 0
                        , _end = Position 4 1000
                        }
      doc <- createDoc "Sigs.hs" "haskell" content
      setConfigSection "haskell" (createConfig False)
      inlayHints <- getInlayHints doc range
      liftIO $ length inlayHints @?= 0
  , testGroup "apply EditText"
    [ editTest "Simple"
    , editTest "Tuple"
    , editTest "Inline"
    , editTest "Infix"
    , editTest "Operator"
    , expectFail $ editTest "ScopedTypeVariables"
    , editTest "Nest"
    , editTest "NoLens"
    , expectFail $ editTest "Typeclass"
    , editTest "Qualified"
    ]
  , testGroup "apply EditText"
    [ hintTest "Simple" $ (@=?)
      [defInlayHint { _position = Position 5 9
                    , _label = InL ":: Bool"
                    , _textEdits = Just [mkTextEdit 5 8 "g :: Bool\n        "]
                    }]
    , hintTest "Tuple" $ (@=?)
      [ defInlayHint { _position = Position 5 10
                     , _label = InL ":: Integer"
                     , _textEdits = Just [mkTextEdit 5 8 "g :: Integer\n        "]
                     }
      , defInlayHint { _position = Position 5 13
                     , _label = InL ":: Bool"
                     , _textEdits = Just [mkTextEdit 5 8 "h :: Bool\n        "]
                     }
      ]
    , hintTest "Inline" $ (@=?)
      [defInlayHint { _position = Position 4 11
                    , _label = InL ":: Bool"
                    , _textEdits = Just [mkTextEdit 4 10 "g :: Bool\n          "]
                    }]
    , hintTest "Infix" $ (@=?)
      [defInlayHint { _position = Position 5 13
                    , _label = InL ":: p1 -> p -> p1"
                    , _textEdits = Just [mkTextEdit 5 8 "g :: p1 -> p -> p1\n        "]
                    }]
    , hintTest "Operator" $ (@=?)
      [defInlayHint { _position = Position 5 9
                    , _label = InL ":: (a -> b) -> a -> b"
                    , _textEdits = Just [mkTextEdit 5 8 "g :: (a -> b) -> a -> b\n        "]
                    }]
    , hintTest "Nest" $ (@=?)
      [ defInlayHint { _position = Position 6 9
                     , _label = InL ":: Int"
                     , _textEdits = Just [mkTextEdit 6 8 "h :: Int\n        "]
                     }
      , defInlayHint { _position = Position 5 9
                     , _label = InL ":: Int"
                     , _textEdits = Just [mkTextEdit 5 8 "g :: Int\n        "]
                     }
      , defInlayHint { _position = Position 6 21
                     , _label = InL ":: Int"
                     , _textEdits = Just [mkTextEdit 6 20 "k :: Int\n                    "]
                     }
      ]
    , hintTest "NoLens" $ (@=?) []
    , hintTest "Qualified" $ (@=?)
      [ defInlayHint { _position = Position 7 10
                     , _label = InL ":: Map.Map Bool Char"
                     , _textEdits = Just [mkTextEdit 7 9 "g :: Map.Map Bool Char\n         "]
                     }
      ]
    ]
  ]

editTest :: String -> TestTree
editTest file =
  testWithDummyPlugin (file <> " (InlayHint EditText)") (mkIdeTestFs [copyDir "local-sig-inlay-hints"]) $ do
    doc <- openDoc (file ++ ".hs") "haskell"
    executeAllHints doc globalRange
    real <- documentContents doc
    expectedDoc <- openDoc (file ++ ".expected.hs") "haskell"
    expected <- documentContents expectedDoc
    liftIO $ real @?= expected

hintTest :: String -> ([InlayHint] -> Assertion) -> TestTree
hintTest file assert =
  testWithDummyPlugin (file <> " (InlayHint)") (mkIdeTestFs [copyDir "local-sig-inlay-hints"]) $ do
    doc <- openDoc (file ++ ".hs") "haskell"
    hints <- getInlayHints doc globalRange
    liftIO $ assert hints


createConfig :: Bool -> A.Value
createConfig on =
  A.object [ "plugin"
    A..= A.object [ "ghcide-type-lenses"
      A..= A.object [ "config"
        A..= A.object [ "localBindingInlayHintOn" A..= A.Bool on ]]]]


executeAllHints :: TextDocumentIdentifier -> Range -> Session ()
executeAllHints doc range = do
    void $ waitForTypecheck doc
    hints <- getInlayHints doc range
    let edits = concat $ mapMaybe _textEdits hints
    case edits of
      [] -> pure ()
      edit : _ -> do
        newDoc <- applyEdit doc edit
        executeAllHints (TextDocumentIdentifier $ _uri newDoc) range

defInlayHint :: InlayHint
defInlayHint =
  InlayHint { _position = Position 0 0
            , _label = InL ""
            , _kind = Nothing
            , _textEdits = Nothing
            , _tooltip = Nothing
            , _paddingLeft = Just True
            , _paddingRight = Nothing
            , _data_ = Nothing
            }

mkTextEdit :: UInt -> UInt -> T.Text -> TextEdit
mkTextEdit x y text =
  TextEdit { _range = pointRange x y
           , _newText = text
           }

pointRange :: UInt -> UInt -> Range
pointRange x y = Range (Position x y) (Position x y)

globalRange :: Range
globalRange = Range { _start = Position 0 0
                    , _end = Position 1000 0
                    }

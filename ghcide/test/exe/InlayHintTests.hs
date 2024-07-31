module InlayHintTests (tests) where

import           Config                      (mkIdeTestFs, testWithDummyPlugin,
                                              testWithDummyPluginEmpty)
import           Control.Monad               (void)
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import qualified Data.Aeson                  as A
import           Data.Maybe                  (mapMaybe)
import qualified Data.Text                   as T
import           Language.LSP.Protocol.Types (InlayHint (_textEdits),
                                              Position (Position),
                                              Range (Range, _end, _start),
                                              TextDocumentIdentifier (TextDocumentIdentifier),
                                              VersionedTextDocumentIdentifier (_uri))
import           Language.LSP.Test           (applyEdit, createDoc,
                                              documentContents, getInlayHints,
                                              openDoc, setConfigSection)
import           Test.Hls                    (Session, expectFail,
                                              waitForTypecheck)
import           Test.Hls.FileSystem         (copyDir)
import           Test.Tasty                  (TestTree, testGroup)
import           Test.Tasty.HUnit            ((@?=))

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
  , editTest "Simple" "Simple"
  , editTest "Tuple" "Tuple"
  , editTest "Inline" "Inline"
  , editTest "Infix" "Infix"
  , editTest "Operator" "Operator"
  , expectFail $ editTest "ScopedTypeVariables" "ScopedTypeVariables"
  , editTest "Nest" "Nest"
  , editTest "No lens" "NoLens"
  , expectFail $ editTest "Typeclass" "Typeclass"
  , editTest "Quqlified" "Qualified"
  ]
  where
    createConfig on =
      A.object [ "plugin"
        A..= A.object [ "ghcide-type-lenses"
          A..= A.object [ "config"
            A..= A.object [ "whereInlayHintOn" A..= A.Bool on ]]]]

    editTest title file =
      testWithDummyPlugin title (mkIdeTestFs [copyDir "local-sig-lens"]) $ do
        doc <- openDoc (file ++ ".hs") "haskell"
        executeAllHints doc globalRange
        real <- documentContents doc
        expectedDoc <- openDoc (file ++ ".expected.hs") "haskell"
        expected <- documentContents expectedDoc
        liftIO $ real @?= expected

    executeAllHints :: TextDocumentIdentifier -> Range -> Session ()
    executeAllHints doc range = do
        void $ waitForTypecheck doc
        hints <- getInlayHints doc range
        let edits = concat $ mapMaybe _textEdits hints
        case edits of
          [] -> pure ()
          edit : _ -> do
            newDoc <- applyEdit doc edit
            -- pure ()
            executeAllHints (TextDocumentIdentifier $ _uri newDoc) range

globalRange :: Range
globalRange = Range { _start = Position 0 0
                    , _end = Position 1000 0
                    }


module HighlightTests (tests) where

import           Control.Monad.IO.Class         (liftIO)
import qualified Data.Text                      as T
import           Development.IDE.GHC.Compat     (GhcVersion (..))
import           Development.IDE.Types.Location
import           Language.LSP.Protocol.Types    hiding
                                                (SemanticTokenAbsolute (..),
                                                 SemanticTokenRelative (..),
                                                 SemanticTokensEdit (..),
                                                 mkRange)
import           Language.LSP.Test
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUtils

tests :: TestTree
tests = testGroup "highlight"
  [ testSessionWait "value" $ do
    doc <- createDoc "A.hs" "haskell" source
    _ <- waitForDiagnostics
    highlights <- getHighlights doc (Position 3 2)
    liftIO $ highlights @?=
            [ DocumentHighlight (R 2 0 2 3) (Just DocumentHighlightKind_Read)
            , DocumentHighlight (R 3 0 3 3) (Just DocumentHighlightKind_Write)
            , DocumentHighlight (R 4 6 4 9) (Just DocumentHighlightKind_Read)
            , DocumentHighlight (R 5 22 5 25) (Just DocumentHighlightKind_Read)
            ]
  , testSessionWait "type" $ do
    doc <- createDoc "A.hs" "haskell" source
    _ <- waitForDiagnostics
    highlights <- getHighlights doc (Position 2 8)
    liftIO $ highlights @?=
            [ DocumentHighlight (R 2 7 2 10) (Just DocumentHighlightKind_Read)
            , DocumentHighlight (R 3 11 3 14) (Just DocumentHighlightKind_Read)
            ]
  , testSessionWait "local" $ do
    doc <- createDoc "A.hs" "haskell" source
    _ <- waitForDiagnostics
    highlights <- getHighlights doc (Position 6 5)
    liftIO $ highlights @?=
            [ DocumentHighlight (R 6 4 6 7) (Just DocumentHighlightKind_Write)
            , DocumentHighlight (R 6 10 6 13) (Just DocumentHighlightKind_Read)
            , DocumentHighlight (R 7 12 7 15) (Just DocumentHighlightKind_Read)
            ]
  , knownBrokenForGhcVersions [GHC92, GHC94, GHC96, GHC98] "Ghc9 highlights the constructor and not just this field" $
        testSessionWait "record" $ do
        doc <- createDoc "A.hs" "haskell" recsource
        _ <- waitForDiagnostics
        highlights <- getHighlights doc (Position 4 15)
        liftIO $ highlights @?=
          [ DocumentHighlight (R 4 8 4 10) (Just DocumentHighlightKind_Write)
          , DocumentHighlight (R 4 14 4 20) (Just DocumentHighlightKind_Read)
          ]
        highlights <- getHighlights doc (Position 3 17)
        liftIO $ highlights @?=
          [ DocumentHighlight (R 3 17 3 23) (Just DocumentHighlightKind_Write)
          , DocumentHighlight (R 4 8 4 10) (Just DocumentHighlightKind_Read)
          ]
  ]
  where
    source = T.unlines
      ["{-# OPTIONS_GHC -Wunused-binds #-}"
      ,"module Highlight () where"
      ,"foo :: Int"
      ,"foo = 3 :: Int"
      ,"bar = foo"
      ,"  where baz = let x = foo in x"
      ,"baz arg = arg + x"
      ,"  where x = arg"
      ]
    recsource = T.unlines
      ["{-# LANGUAGE RecordWildCards #-}"
      ,"{-# OPTIONS_GHC -Wunused-binds #-}"
      ,"module Highlight () where"
      ,"data Rec = Rec { field1 :: Int, field2 :: Char }"
      ,"foo Rec{..} = field2 + field1"
      ]

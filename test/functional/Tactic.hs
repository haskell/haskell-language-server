{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Tactic
  ( tests
  )
where

import Debug.Trace
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import           Control.Applicative.Combinators
                                                ( skipManyTill )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import qualified Data.Text.IO                  as T
import           Language.Haskell.LSP.Test
import           Language.Haskell.LSP.Types     ( ApplyWorkspaceEditRequest
                                                , CodeLens
                                                , Command(_title)
                                                , Position(..)
                                                , Range(..)
                                                , Location(Location)
                                                , CAResult(..)
                                                , CodeAction(..)
                                                , Uri
                                                )
import           System.FilePath
import           Test.Hls.Util
import           Test.Tasty
import           Test.Tasty.ExpectedFailure (expectFailBecause)
import           Test.Tasty.HUnit
import Data.List
import Data.Maybe (listToMaybe, mapMaybe)
import Ide.Plugin.Tactic (tacticTitle, TacticCommand (..))


pointRange :: Int -> Int -> Range
pointRange
  (subtract 1 -> line)
  (subtract 1 -> col) =
    Range (Position line col) (Position line $ col + 1)

codeActionTitle :: CAResult -> Maybe Text
codeActionTitle CACommand{} = Nothing
codeActionTitle (CACodeAction(CodeAction title _ _ _ _)) = Just title

tests :: TestTree
tests = testGroup
  "tactic"
  [ mkTest
      "Produces intros code action"
      "T1.hs" 2 14
      [ (id, Intros, "")
      ]
  , mkTest
      "Produces destruct and homomorphism code actions"
      "T2.hs" 2 21
      [ (id, Destruct, "eab")
      , (id, Homomorphism, "eab")
      ]
  , mkTest
      "Can destruct globals"
      "T2.hs" 8 8
      [ (id, Destruct, "global")
      ]
  , mkTest
      "Won't suggest homomorphism on the wrong type"
      "T2.hs" 8 8
      [ (not, Homomorphism, "global")
      ]
  , mkTest
      "Won't suggest intros on the wrong type"
      "T2.hs" 8 8
      [ (not, Intros, "")
      ]
  ]

mkTest
    :: Foldable t
    => String
    -> FilePath
    -> Int
    -> Int
    -> t ( Bool -> Bool   -- Use 'not' for actions that shouldnt be present
         , TacticCommand
         , Text
         )
    -> TestTree
mkTest name fp line col ts =
  testCase name $ do
  runSession hieCommand fullCaps tacticPath $ do
    doc <- openDoc fp "haskell"
    actions <- getCodeActions doc $ pointRange line col
    let titles = mapMaybe codeActionTitle actions
    for_ ts $ \(f, tc, var) -> do
      let title = tacticTitle tc var
      liftIO $
        f (elem title titles)
          @? ("Expected a code action with title " <> T.unpack title)

tacticPath :: FilePath
tacticPath = "test/testdata/tactic"


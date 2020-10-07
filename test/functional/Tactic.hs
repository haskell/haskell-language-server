{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Tactic
  ( tests
  )
where

import           Control.Applicative.Combinators ( skipManyTill )
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Ide.Plugin.Tactic.TestTypes
import           Language.Haskell.LSP.Test
import           Language.Haskell.LSP.Types (ApplyWorkspaceEditRequest, Position(..) , Range(..) , CAResult(..) , CodeAction(..))
import           Test.Hls.Util
import           Test.Tasty
import           Test.Tasty.HUnit
import           System.FilePath
import System.Directory (doesFileExist)
import Control.Monad (unless)


------------------------------------------------------------------------------
-- | Get a range at the given line and column corresponding to having nothing
-- selected.
--
-- NB: These coordinates are in "file space", ie, 1-indexed.
pointRange :: Int -> Int -> Range
pointRange
  (subtract 1 -> line)
  (subtract 1 -> col) =
    Range (Position line col) (Position line $ col + 1)


------------------------------------------------------------------------------
-- | Get the title of a code action.
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
      "Won't suggest homomorphism on the wrong type"
      "T2.hs" 8 8
      [ (not, Homomorphism, "global")
      ]
  , mkTest
      "Won't suggest intros on the wrong type"
      "T2.hs" 8 8
      [ (not, Intros, "")
      ]
  , mkTest
      "Produces (homomorphic) lambdacase code actions"
      "T3.hs" 4 24
      [ (id, HomomorphismLambdaCase, "")
      , (id, DestructLambdaCase, "")
      ]
  , mkTest
      "Produces lambdacase code actions"
      "T3.hs" 7 13
      [ (id, DestructLambdaCase, "")
      ]
  , mkTest
      "Doesn't suggest lambdacase without -XLambdaCase"
      "T2.hs" 11 25
      [ (not, DestructLambdaCase, "")
      ]
  , goldenTest "GoldenIntros.hs"            2 8  Intros ""
  , goldenTest "GoldenEitherAuto.hs"        2 11 Auto   ""
  , goldenTest "GoldenJoinCont.hs"          4 12 Auto   ""
  , goldenTest "GoldenIdentityFunctor.hs"   3 11 Auto   ""
  , goldenTest "GoldenIdTypeFam.hs"         7 11 Auto   ""
  , goldenTest "GoldenEitherHomomorphic.hs" 2 15 Auto ""
  , goldenTest "GoldenNote.hs"              2 8  Auto ""
  , goldenTest "GoldenPureList.hs"          2 12 Auto ""
  , goldenTest "GoldenGADTDestruct.hs"      7 17 Destruct "gadt"
  , goldenTest "GoldenGADTAuto.hs"          7 13 Auto ""
  ]


------------------------------------------------------------------------------
-- | Make a tactic unit test.
mkTest
    :: Foldable t
    => String  -- ^ The test name
    -> FilePath  -- ^ The file to load
    -> Int  -- ^ Cursor line
    -> Int  -- ^ Cursor columnn
    -> t ( Bool -> Bool   -- Use 'not' for actions that shouldnt be present
         , TacticCommand  -- An expected command ...
         , Text           -- ... for this variable
         ) -- ^ A collection of (un)expected code actions.
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


goldenTest :: FilePath -> Int -> Int -> TacticCommand -> Text -> TestTree
goldenTest input line col tc occ =
  testCase (input <> " (golden)") $ do
    runSession hieCommand fullCaps tacticPath $ do
      doc <- openDoc input "haskell"
      actions <- getCodeActions doc $ pointRange line col
      Just (CACodeAction (CodeAction {_command = Just c}))
        <- pure $ find ((== Just (tacticTitle tc occ)) . codeActionTitle) actions
      executeCommand c
      _resp :: ApplyWorkspaceEditRequest <- skipManyTill anyMessage message
      edited <- documentContents doc
      let expected_name = tacticPath </> input <.> "expected"
      -- Write golden tests if they don't already exist
      liftIO $ (doesFileExist expected_name >>=) $ flip unless $ do
        T.writeFile expected_name edited
      expected <- liftIO $ T.readFile expected_name
      liftIO $ edited @?= expected


tacticPath :: FilePath
tacticPath = "test/testdata/tactic"


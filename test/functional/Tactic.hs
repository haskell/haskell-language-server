{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Tactic
  ( tests
  )
where

import           Control.Applicative.Combinators ( skipManyTill )
import           Control.Lens hiding ((<.>))
import           Control.Monad (unless)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Default (Default(def))
import           Data.Either (isLeft)
import           Data.Foldable
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Ide.Plugin.Config as Plugin
import           Ide.Plugin.Tactic.FeatureSet (FeatureSet, allFeatures)
import           Ide.Plugin.Tactic.TestTypes
import           Language.LSP.Test
import           Language.LSP.Types
import           Language.LSP.Types.Lens hiding (id, capabilities, message, executeCommand, applyEdit, rename)
import           System.Directory (doesFileExist)
import           System.FilePath
import           Test.Hls.Util
import           Test.Tasty
import           Test.Tasty.ExpectedFailure (ignoreTestBecause)
import           Test.Tasty.HUnit


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
codeActionTitle :: (Command |? CodeAction) -> Maybe Text
codeActionTitle InL{} = Nothing
codeActionTitle (InR(CodeAction title _ _ _ _ _ _)) = Just title


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
  , goldenTest "GoldenEitherAuto.hs"        2 11 Auto ""
  , goldenTest "GoldenJoinCont.hs"          4 12 Auto ""
  , goldenTest "GoldenIdentityFunctor.hs"   3 11 Auto ""
  , goldenTest "GoldenIdTypeFam.hs"         7 11 Auto ""
  , goldenTest "GoldenEitherHomomorphic.hs" 2 15 Auto ""
  , goldenTest "GoldenNote.hs"              2 8  Auto ""
  , goldenTest "GoldenPureList.hs"          2 12 Auto ""
  , goldenTest "GoldenListFmap.hs"          2 12 Auto ""
  , goldenTest "GoldenFromMaybe.hs"         2 13 Auto ""
  , goldenTest "GoldenFoldr.hs"             2 10 Auto ""
  , goldenTest "GoldenSwap.hs"              2 8  Auto ""
  , goldenTest "GoldenFmapTree.hs"          4 11 Auto ""
  , goldenTest "GoldenGADTDestruct.hs"      7 17 Destruct "gadt"
  , goldenTest "GoldenGADTDestructCoercion.hs" 8 17 Destruct "gadt"
  , goldenTest "GoldenGADTAuto.hs"          7 13 Auto ""
  , goldenTest "GoldenSwapMany.hs"          2 12 Auto ""
  , goldenTest "GoldenBigTuple.hs"          4 12 Auto ""
  , goldenTest "GoldenShow.hs"              2 10 Auto ""
  , goldenTest "GoldenShowCompose.hs"       2 15 Auto ""
  , goldenTest "GoldenShowMapChar.hs"       2 8  Auto ""
  , goldenTest "GoldenSuperclass.hs"        7 8  Auto ""
  , ignoreTestBecause "It is unreliable in circleci builds"
      $ goldenTest "GoldenApplicativeThen.hs"   2 11 Auto ""
  , goldenTest "GoldenSafeHead.hs"          2 12 Auto ""
  , expectFail "GoldenFish.hs"              5 18 Auto ""
  , goldenTest "GoldenArbitrary.hs"         25 13 Auto ""
  , goldenTest "FmapBoth.hs"                2 12 Auto ""
  , goldenTest "RecordCon.hs"               7  8 Auto ""
  , goldenTest "FmapJoin.hs"                2 14 Auto ""
  , goldenTest "Fgmap.hs"                   2 9  Auto ""
  , goldenTest "FmapJoinInLet.hs"           4 19 Auto ""
  , goldenTest "SplitPattern.hs"            7 25 Destruct "a"
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
  runSession hlsCommand fullCaps tacticPath $ do
    doc <- openDoc fp "haskell"
    _ <- waitForDiagnostics
    actions <- getCodeActions doc $ pointRange line col
    let titles = mapMaybe codeActionTitle actions
    for_ ts $ \(f, tc, var) -> do
      let title = tacticTitle tc var
      liftIO $
        f (title `elem` titles)
          @? ("Expected a code action with title " <> T.unpack title)


setFeatureSet :: FeatureSet -> Session ()
setFeatureSet features = do
  let unObject (Object obj) = obj
      unObject _ = undefined
      def_config = def :: Plugin.Config
      config =
        def_config
          { Plugin.plugins = M.fromList [("tactics",
              def { Plugin.plcConfig = unObject $ toJSON $
                emptyConfig { cfg_feature_set = features }}
          )] <> Plugin.plugins def_config }

  sendNotification SWorkspaceDidChangeConfiguration $
    DidChangeConfigurationParams $
      toJSON config

goldenTest :: FilePath -> Int -> Int -> TacticCommand -> Text -> TestTree
goldenTest = goldenTest' allFeatures

goldenTest' :: FeatureSet -> FilePath -> Int -> Int -> TacticCommand -> Text -> TestTree
goldenTest' features input line col tc occ =
  testCase (input <> " (golden)") $ do
    runSession hlsCommand fullCaps tacticPath $ do
      setFeatureSet features
      doc <- openDoc input "haskell"
      _ <- waitForDiagnostics
      actions <- getCodeActions doc $ pointRange line col
      Just (InR CodeAction {_command = Just c})
        <- pure $ find ((== Just (tacticTitle tc occ)) . codeActionTitle) actions
      executeCommand c
      _resp <- skipManyTill anyMessage (message SWorkspaceApplyEdit)
      edited <- documentContents doc
      let expected_name = tacticPath </> input <.> "expected"
      -- Write golden tests if they don't already exist
      liftIO $ (doesFileExist expected_name >>=) $ flip unless $ do
        T.writeFile expected_name edited
      expected <- liftIO $ T.readFile expected_name
      liftIO $ edited @?= expected


expectFail :: FilePath -> Int -> Int -> TacticCommand -> Text -> TestTree
expectFail input line col tc occ =
  testCase (input <> " (golden)") $ do
    runSession hlsCommand fullCaps tacticPath $ do
      doc <- openDoc input "haskell"
      _ <- waitForDiagnostics
      actions <- getCodeActions doc $ pointRange line col
      Just (InR CodeAction {_command = Just c})
        <- pure $ find ((== Just (tacticTitle tc occ)) . codeActionTitle) actions
      resp <- executeCommandWithResp c
      liftIO $ unless (isLeft $ _result resp) $
        assertFailure "didn't fail, but expected one"


tacticPath :: FilePath
tacticPath = "test/testdata/tactic"


executeCommandWithResp :: Command -> Session (ResponseMessage WorkspaceExecuteCommand)
executeCommandWithResp cmd = do
  let args = decode $ encode $ fromJust $ cmd ^. arguments
      execParams = ExecuteCommandParams Nothing (cmd ^. command) args
  request SWorkspaceExecuteCommand execParams

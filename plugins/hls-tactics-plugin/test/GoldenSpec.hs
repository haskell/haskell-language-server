{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module GoldenSpec where

import           Control.Applicative.Combinators (skipManyTill)
import           Control.Lens hiding (failing, (<.>))
import           Control.Monad (unless)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Default (Default (def))
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
import           Language.LSP.Types.Lens hiding (actions, applyEdit, capabilities, executeCommand, id, line, message, name, rename, title)
import           System.Directory (doesFileExist)
import           System.FilePath
import           Test.Hspec


spec :: Spec
spec = do
  describe "code action availability" $ do
    mkTest
      "Produces intros code action"
      "T1.hs" 2 14
      [ (id, Intros, "")
      ]
    mkTest
      "Produces destruct and homomorphism code actions"
      "T2.hs" 2 21
      [ (id, Destruct, "eab")
      , (id, Homomorphism, "eab")
      ]
    mkTest
      "Won't suggest homomorphism on the wrong type"
      "T2.hs" 8 8
      [ (not, Homomorphism, "global")
      ]
    mkTest
      "Won't suggest intros on the wrong type"
      "T2.hs" 8 8
      [ (not, Intros, "")
      ]
    mkTest
      "Produces (homomorphic) lambdacase code actions"
      "T3.hs" 4 24
      [ (id, HomomorphismLambdaCase, "")
      , (id, DestructLambdaCase, "")
      ]
    mkTest
      "Produces lambdacase code actions"
      "T3.hs" 7 13
      [ (id, DestructLambdaCase, "")
      ]
    mkTest
      "Doesn't suggest lambdacase without -XLambdaCase"
      "T2.hs" 11 25
      [ (not, DestructLambdaCase, "")
      ]

  let goldenTest = mkGoldenTest allFeatures

  -- test via:
  -- stack test hls-tactics-plugin --test-arguments '--match "Golden/use constructor/"'
  describe "use constructor" $ do
    let useTest = mkGoldenTest allFeatures UseDataCon
    describe "provider" $ do
      mkTest
        "Suggests all data cons for Either"
        "ConProviders.hs" 5 6
        [ (id, UseDataCon, "Left")
        , (id, UseDataCon, "Right")
        , (not, UseDataCon, ":")
        , (not, UseDataCon, "[]")
        , (not, UseDataCon, "C1")
        ]
      mkTest
        "Suggests no data cons for big types"
        "ConProviders.hs" 11 17 $ do
          c <- [1 :: Int .. 10]
          pure $ (not, UseDataCon, T.pack $ show c)
      mkTest
        "Suggests only matching data cons for GADT"
        "ConProviders.hs" 20 12
        [ (id, UseDataCon, "IntGADT")
        , (id, UseDataCon, "VarGADT")
        , (not, UseDataCon, "BoolGADT")
        ]

    describe "golden" $ do
      useTest "(,)"   "UseConPair.hs"  2 8
      useTest "Left"  "UseConLeft.hs"  2 8
      useTest "Right" "UseConRight.hs" 2 8

  -- test via:
  -- stack test hls-tactics-plugin --test-arguments '--match "Golden/refine/"'
  describe "refine" $ do
    let refineTest = mkGoldenTest allFeatures Refine ""
    describe "golden" $ do
      refineTest "RefineIntro.hs"  2 8
      refineTest "RefineCon.hs"    2 8
      refineTest "RefineReader.hs" 4 8
      refineTest "RefineGADT.hs"   8 8


  describe "golden tests" $ do
    let autoTest = mkGoldenTest allFeatures Auto ""

    goldenTest Intros "" "GoldenIntros.hs" 2 8
    autoTest "GoldenEitherAuto.hs"        2 11
    autoTest "GoldenJoinCont.hs"          4 12
    autoTest "GoldenIdentityFunctor.hs"   3 11
    autoTest "GoldenIdTypeFam.hs"         7 11
    autoTest "GoldenEitherHomomorphic.hs" 2 15
    autoTest "GoldenNote.hs"              2 8
    autoTest "GoldenPureList.hs"          2 12
    autoTest "GoldenListFmap.hs"          2 12
    autoTest "GoldenFromMaybe.hs"         2 13
    autoTest "GoldenFoldr.hs"             2 10
    autoTest "GoldenSwap.hs"              2 8
    autoTest "GoldenFmapTree.hs"          4 11
    goldenTest Destruct "gadt"
             "GoldenGADTDestruct.hs"      7 17
    goldenTest Destruct "gadt"
             "GoldenGADTDestructCoercion.hs" 8 17
    autoTest "GoldenGADTAuto.hs"    7 13
    autoTest "GoldenSwapMany.hs"    2 12
    autoTest "GoldenBigTuple.hs"    4 12
    autoTest "GoldenShow.hs"        2 10
    autoTest "GoldenShowCompose.hs" 2 15
    autoTest "GoldenShowMapChar.hs" 2 8
    autoTest "GoldenSuperclass.hs"  7 8
    failing "flaky in CI" $
      autoTest "GoldenApplicativeThen.hs" 2 11
    autoTest "GoldenSafeHead.hs"  2 12
    failing "not enough auto gas" $
      autoTest "GoldenFish.hs" 5 18
    autoTest "GoldenArbitrary.hs" 25 13
    autoTest "FmapBoth.hs"        2 12
    autoTest "RecordCon.hs"       7  8
    autoTest "FmapJoin.hs"        2 14
    autoTest "Fgmap.hs"           2 9
    autoTest "FmapJoinInLet.hs"   4 19
    goldenTest Destruct "a"
      "SplitPattern.hs"  7 25


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
codeActionTitle InL{}                               = Nothing
codeActionTitle (InR(CodeAction title _ _ _ _ _ _)) = Just title


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
    -> SpecWith (Arg Bool)
mkTest name fp line col ts = it name $ do
  runSession testCommand fullCaps tacticPath $ do
    setFeatureSet allFeatures
    doc <- openDoc fp "haskell"
    _ <- waitForDiagnostics
    actions <- getCodeActions doc $ pointRange line col
    let titles = mapMaybe codeActionTitle actions
    for_ ts $ \(f, tc, var) -> do
      let title = tacticTitle tc var
      liftIO $
        (title `elem` titles) `shouldSatisfy` f


setFeatureSet :: FeatureSet -> Session ()
setFeatureSet features = do
  let unObject (Object obj) = obj
      unObject _            = undefined
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


mkGoldenTest
    :: FeatureSet
    -> TacticCommand
    -> Text
    -> FilePath
    -> Int
    -> Int
    -> SpecWith ()
mkGoldenTest features tc occ input line col =
  it (input <> " (golden)") $ do
    runSession testCommand fullCaps tacticPath $ do
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
      liftIO $ edited `shouldBe` expected


------------------------------------------------------------------------------
-- | Don't run a test.
failing :: Applicative m => String -> b -> m ()
failing _ _ = pure ()


tacticPath :: FilePath
tacticPath = "test/golden"


testCommand :: String
testCommand = "test-server"


executeCommandWithResp :: Command -> Session (ResponseMessage 'WorkspaceExecuteCommand)
executeCommandWithResp cmd = do
  let args = decode $ encode $ fromJust $ cmd ^. arguments
      execParams = ExecuteCommandParams Nothing (cmd ^. command) args
  request SWorkspaceExecuteCommand execParams


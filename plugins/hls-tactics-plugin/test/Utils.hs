{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module Utils where

import           Control.Applicative.Combinators (skipManyTill)
import           Control.Lens hiding (failing, (<.>), (.=))
import           Control.Monad (unless)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Default (Default (def))
import           Data.Foldable
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Ide.Plugin.Config as Plugin
import           Wingman.FeatureSet (FeatureSet, allFeatures, prettyFeatureSet)
import           Wingman.LanguageServer (mkShowMessageParams)
import           Wingman.Types
import           Language.LSP.Test
import           Language.LSP.Types
import           Language.LSP.Types.Lens hiding (actions, applyEdit, capabilities, executeCommand, id, line, message, name, rename, title)
import           System.Directory (doesFileExist)
import           System.FilePath
import           Test.Hspec


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
codeActionTitle (InR(CodeAction title _ _ _ _ _ _ _)) = Just title


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
              def { Plugin.plcConfig = unObject $ object ["features" .= prettyFeatureSet features] }
          )] <> Plugin.plugins def_config }

  sendNotification SWorkspaceDidChangeConfiguration $
    DidChangeConfigurationParams $
      toJSON config


mkGoldenTest
    :: FeatureSet
    -> TacticCommand
    -> Text
    -> Int
    -> Int
    -> FilePath
    -> SpecWith ()
mkGoldenTest features tc occ line col input =
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

mkShowMessageTest
    :: FeatureSet
    -> TacticCommand
    -> Text
    -> Int
    -> Int
    -> FilePath
    -> UserFacingMessage
    -> SpecWith ()
mkShowMessageTest features tc occ line col input ufm =
  it (input <> " (golden)") $ do
    runSession testCommand fullCaps tacticPath $ do
      setFeatureSet features
      doc <- openDoc input "haskell"
      _ <- waitForDiagnostics
      actions <- getCodeActions doc $ pointRange line col
      Just (InR CodeAction {_command = Just c})
        <- pure $ find ((== Just (tacticTitle tc occ)) . codeActionTitle) actions
      executeCommand c
      NotificationMessage _ _ err <- skipManyTill anyMessage (message SWindowShowMessage)
      liftIO $ err `shouldBe` mkShowMessageParams ufm


goldenTest :: TacticCommand -> Text -> Int -> Int -> FilePath -> SpecWith ()
goldenTest = mkGoldenTest allFeatures


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


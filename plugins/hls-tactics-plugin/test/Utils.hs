{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module Utils where

import           Control.DeepSeq (deepseq)
import qualified Control.Exception as E
import           Control.Lens hiding (List, failing, (<.>), (.=))
import           Control.Monad (unless, void)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Foldable
import           Data.Function (on)
import           Data.IORef (writeIORef)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Ide.Plugin.Tactic as Tactic
import           Language.LSP.Types
import           Language.LSP.Types.Lens hiding (actions, applyEdit, capabilities, executeCommand, id, line, message, name, rename, title)
import           System.Directory (doesFileExist)
import           System.FilePath
import           Test.Hls
import           Test.Hspec
import           Test.Hspec.Formatters (FailureReason(ExpectedButGot))
import           Wingman.LanguageServer (mkShowMessageParams)
import           Wingman.Types


plugin :: PluginDescriptor IdeState
plugin = Tactic.descriptor "tactics"

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


resetGlobalHoleRef :: IO ()
resetGlobalHoleRef = writeIORef globalHoleRef 0


runSessionForTactics :: Session a -> IO a
runSessionForTactics =
  runSessionWithServer'
    [plugin]
    def
    (def { messageTimeout = 5 } )
    fullCaps
    tacticPath

------------------------------------------------------------------------------
-- | Make a tactic unit test.
mkTest
    :: Foldable t
    => String  -- ^ The test name
    -> FilePath  -- ^ The file name stem (without extension) to load
    -> Int  -- ^ Cursor line
    -> Int  -- ^ Cursor columnn
    -> t ( Bool -> Bool   -- Use 'not' for actions that shouldnt be present
         , TacticCommand  -- An expected command ...
         , Text           -- ... for this variable
         ) -- ^ A collection of (un)expected code actions.
    -> SpecWith (Arg Bool)
mkTest name fp line col ts = it name $ do
  resetGlobalHoleRef
  runSessionForTactics $ do
    doc <- openDoc (fp <.> "hs") "haskell"
    -- wait for diagnostics to start coming
    void waitForDiagnostics
    -- wait for the entire build to finish, so that Tactics code actions that
    -- use stale data will get uptodate stuff
    void $ waitForTypecheck doc
    actions <- getCodeActions doc $ pointRange line col
    let titles = mapMaybe codeActionTitle actions
    for_ ts $ \(f, tc, var) -> do
      let title = tacticTitle tc var
      liftIO $
        (title `elem` titles) `shouldSatisfy` f



mkGoldenTest
    :: (Text -> Text -> Assertion)
    -> TacticCommand
    -> Text
    -> Int
    -> Int
    -> FilePath
    -> SpecWith ()
mkGoldenTest eq tc occ line col input =
  it (input <> " (golden)") $ do
    resetGlobalHoleRef
    runSessionForTactics $ do
      doc <- openDoc (input <.> "hs") "haskell"
      -- wait for diagnostics to start coming
      void waitForDiagnostics
      -- wait for the entire build to finish, so that Tactics code actions that
      -- use stale data will get uptodate stuff
      void $ waitForTypecheck doc
      actions <- getCodeActions doc $ pointRange line col
      case find ((== Just (tacticTitle tc occ)) . codeActionTitle) actions of
        Just (InR CodeAction {_command = Just c}) -> do
            executeCommand c
            _resp <- skipManyTill anyMessage (message SWorkspaceApplyEdit)
            edited <- documentContents doc
            let expected_name = input <.> "expected" <.> "hs"
            -- Write golden tests if they don't already exist
            liftIO $ (doesFileExist expected_name >>=) $ flip unless $ do
                T.writeFile expected_name edited
            expected <- liftIO $ T.readFile expected_name
            liftIO $ edited `eq` expected
        _ -> error $ show actions

mkCodeLensTest
    :: FilePath
    -> SpecWith ()
mkCodeLensTest input =
  it (input <> " (golden)") $ do
    resetGlobalHoleRef
    runSessionForTactics $ do
      doc <- openDoc (input <.> "hs") "haskell"
      _ <- waitForDiagnostics
      lenses <- fmap (reverse . filter isWingmanLens) $ getCodeLenses doc
      for_ lenses $ \(CodeLens _ (Just cmd) _) ->
        executeCommand cmd
      _resp <- skipManyTill anyMessage (message SWorkspaceApplyEdit)
      edited <- documentContents doc
      let expected_name = input <.> "expected" <.> "hs"
      -- Write golden tests if they don't already exist
      liftIO $ (doesFileExist expected_name >>=) $ flip unless $ do
        T.writeFile expected_name edited
      expected <- liftIO $ T.readFile expected_name
      liftIO $ edited `shouldBe` expected


------------------------------------------------------------------------------
-- | A test that no code lenses can be run in the file
mkNoCodeLensTest
    :: FilePath
    -> SpecWith ()
mkNoCodeLensTest input =
  it (input <> " (no code lenses)") $ do
    resetGlobalHoleRef
    runSessionForTactics $ do
      doc <- openDoc (input <.> "hs") "haskell"
      _ <- waitForDiagnostics
      lenses <- fmap (reverse . filter isWingmanLens) $ getCodeLenses doc
      liftIO $ lenses `shouldBe` []



isWingmanLens :: CodeLens -> Bool
isWingmanLens (CodeLens _ (Just (Command _ cmd _)) _)
    = T.isInfixOf ":tactics:" cmd
isWingmanLens _ = False


mkShowMessageTest
    :: TacticCommand
    -> Text
    -> Int
    -> Int
    -> FilePath
    -> UserFacingMessage
    -> SpecWith ()
mkShowMessageTest tc occ line col input ufm =
  it (input <> " (golden)") $ do
    resetGlobalHoleRef
    runSessionForTactics $ do
      doc <- openDoc (input <.> "hs") "haskell"
      _ <- waitForDiagnostics
      actions <- getCodeActions doc $ pointRange line col
      Just (InR CodeAction {_command = Just c})
        <- pure $ find ((== Just (tacticTitle tc occ)) . codeActionTitle) actions
      executeCommand c
      NotificationMessage _ _ err <- skipManyTill anyMessage (message SWindowShowMessage)
      liftIO $ err `shouldBe` mkShowMessageParams ufm


goldenTest :: TacticCommand -> Text -> Int -> Int -> FilePath -> SpecWith ()
goldenTest = mkGoldenTest shouldBe

goldenTestNoWhitespace :: TacticCommand -> Text -> Int -> Int -> FilePath -> SpecWith ()
goldenTestNoWhitespace = mkGoldenTest shouldBeIgnoringSpaces


shouldBeIgnoringSpaces :: Text -> Text -> Assertion
shouldBeIgnoringSpaces = assertFun f ""
  where
    f = (==) `on` T.unwords . T.words


assertFun
    :: Show a
    => (a -> a -> Bool)
    -> String -- ^ The message prefix
    -> a      -- ^ The expected value
    -> a      -- ^ The actual value
    -> Assertion
assertFun eq preface expected actual =
  unless (eq actual expected) $ do
    (prefaceMsg
      `deepseq` expectedMsg
      `deepseq` actualMsg
      `deepseq`
        E.throwIO
          (HUnitFailure Nothing $ show $ ExpectedButGot prefaceMsg expectedMsg actualMsg))
  where
    prefaceMsg
      | null preface = Nothing
      | otherwise = Just preface
    expectedMsg = show expected
    actualMsg = show actual



------------------------------------------------------------------------------
-- | Don't run a test.
failing :: Applicative m => String -> b -> m ()
failing _ _ = pure ()


tacticPath :: FilePath
tacticPath = "test/golden"


executeCommandWithResp :: Command -> Session (ResponseMessage 'WorkspaceExecuteCommand)
executeCommandWithResp cmd = do
  let args = decode $ encode $ fromJust $ cmd ^. arguments
      execParams = ExecuteCommandParams Nothing (cmd ^. command) args
  request SWorkspaceExecuteCommand execParams


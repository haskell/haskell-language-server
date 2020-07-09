{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

-- | A plugin inspired by the REPLoid feature of Dante[1] which allows
--     to evaluate code in comment prompts and splice the results right below:
--
--    > example :: [String]
--    > example = ["This is an example", "of", "interactive", "evaluation"]
--    >
--    > -- >>> intercalate " " example
--    > -- "This is an example of interactive evaluation"
--    > --
--
--    [1] - https://github.com/jyp/dante
module Ide.Plugin.Eval where

import           Control.Monad                  (void)
import           Control.Monad.Catch            (finally)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.Class      (MonadTrans (lift))
import           Control.Monad.Trans.Except     (ExceptT (..), runExceptT,
                                                 throwE)
import           Data.Aeson                     (FromJSON, ToJSON, Value (Null),
                                                 toJSON)
import           Data.Bifunctor                 (Bifunctor (first))
import qualified Data.HashMap.Strict            as Map
import qualified Data.Rope.UTF16                as Rope
import           Data.String                    (IsString (fromString))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Time                      (getCurrentTime)
import           Development.IDE.Core.Rules     (runAction)
import           Development.IDE.Core.RuleTypes (GetModSummary (..),
                                                 GhcSessionDeps (..))
import           Development.IDE.Core.Shake     (use_)
import           Development.IDE.GHC.Util       (evalGhcEnv, hscEnv,
                                                 textToStringBuffer)
import           Development.IDE.Types.Location (toNormalizedFilePath',
                                                 uriToFilePath')
import           DynamicLoading                 (initializePlugins)
import           GHC
import           GHC.Generics                   (Generic)
import           GhcMonad                       (modifySession)
import           GhcPlugins                     (defaultLogActionHPutStrDoc,
                                                 gopt_set, gopt_unset,
                                                 interpWays, updateWays,
                                                 wayGeneralFlags,
                                                 wayUnsetGeneralFlags)
import           HscTypes
import           Ide.Plugin
import           Ide.Types
import           Language.Haskell.LSP.Core      (LspFuncs (getVirtualFileFunc))
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.VFS       (VirtualFile (..))
import           PrelNames                      (pRELUDE)
import           System.IO                      (IOMode (WriteMode), hClose, openFile)
import           System.IO.Extra                (newTempFile)

descriptor :: PluginId -> PluginDescriptor
descriptor plId =
  (defaultPluginDescriptor plId)
    { pluginId = plId,
      pluginCodeLensProvider = Just provider,
      pluginCommands = [evalCommand]
    }

extractMatches :: Maybe Text -> [([(Text, Int)], Range)]
extractMatches = goSearch 0 . maybe [] T.lines
  where
    checkMatch = T.stripPrefix "-- >>> "
    looksLikeSplice l
      | Just l' <- T.stripPrefix "--" l
      = not (" >>>" `T.isPrefixOf` l')
      | otherwise
      = False

    goSearch _ [] = []
    goSearch line (l : ll)
      | Just match <- checkMatch l =
        goAcc (line + 1) [(match, line)] ll
      | otherwise =
        goSearch (line + 1) ll

    goAcc line acc [] = [(reverse acc,Range p p)] where p = Position line 0
    goAcc line acc (l:ll)
      | Just match <- checkMatch l =
        goAcc (line + 1) ([(match, line)] <> acc) ll
      | otherwise =
        (reverse acc,r) : goSearch (line + 1) ll
      where
        r = Range p p'
        p = Position line 0
        p' = Position (line + spliceLength) 0
        spliceLength = length (takeWhile looksLikeSplice (l:ll))

provider :: CodeLensProvider
provider lsp _state plId CodeLensParams {_textDocument} = response $ do
  let TextDocumentIdentifier uri = _textDocument
  contents <- liftIO $ getVirtualFileFunc lsp $ toNormalizedUri uri
  let text = Rope.toText . (_text :: VirtualFile -> Rope.Rope) <$> contents
  let matches = extractMatches text

  cmd <- liftIO $ mkLspCommand plId evalCommandName "Evaluate..." (Just [])

  let lenses =
        [ CodeLens range (Just cmd') Nothing
          | (m, r) <- matches,
            let (_, startLine) = head m
                (_, endLine) = last m
                range = Range start end
                start = Position startLine 0
                end = Position endLine 1000
                args = EvalParams m r _textDocument,
            let cmd' = (cmd :: Command)
                    {_arguments = Just (List [toJSON args])
                    ,_title = if trivial r then "Evaluate..." else "Refresh..."
                    }
        ]

  return $ List lenses
  where
      trivial (Range p p') = p == p'

evalCommandName :: CommandId
evalCommandName = "evalCommand"

evalCommand :: PluginCommand
evalCommand =
  PluginCommand evalCommandName "evaluate" runEvalCmd

data EvalParams = EvalParams
  { statements :: [(Text, Int)],
    editTarget :: !Range,
    module_    :: !TextDocumentIdentifier
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

runEvalCmd :: CommandFunction EvalParams
runEvalCmd lsp state EvalParams {..} = response' $ do
  let TextDocumentIdentifier {_uri} = module_
  fp <- handleMaybe "uri" $ uriToFilePath' _uri
  contents <- liftIO $ getVirtualFileFunc lsp $ toNormalizedUri _uri
  text <- handleMaybe "contents" $ Rope.toText . (_text :: VirtualFile -> Rope.Rope) <$> contents

  session <-
    liftIO
      $ runAction "runEvalCmd.ghcSession" state
      $ use_ GhcSessionDeps
      $ toNormalizedFilePath'
      $ fp

  ms <-
    liftIO
      $ runAction "runEvalCmd.getModSummary" state
      $ use_ GetModSummary
      $ toNormalizedFilePath'
      $ fp

  now <- liftIO getCurrentTime

  (temp, clean) <- liftIO newTempFile
  (tempLog, cleanLog) <- liftIO newTempFile
  hLog <- liftIO $ openFile tempLog WriteMode
  flip finally (liftIO $ hClose hLog >> cleanLog >> clean) $ do
    let modName = moduleName $ ms_mod ms
        thisModuleTarget = Target (TargetFile fp Nothing) False (Just (textToStringBuffer text, now))

    hscEnv' <- ExceptT $ evalGhcEnv (hscEnv session) $ do
      df <- getSessionDynFlags
      env <- getSession
      df <- liftIO $ setupDynFlagsForGHCiLike env df
      _lp <- setSessionDynFlags df

      -- copy the package state to the interactive DynFlags
      idflags <- getInteractiveDynFlags
      df <- getSessionDynFlags
      setInteractiveDynFlags
        idflags
          { pkgState = pkgState df,
            pkgDatabase = pkgDatabase df,
            packageFlags = packageFlags df
          }

      -- set up a custom log action
      setLogAction $ \_df _wr _sev _span _style _doc ->
        defaultLogActionHPutStrDoc _df hLog _doc _style

      -- load the module in the interactive environment
      setTargets [thisModuleTarget]
      loadResult <- load LoadAllTargets
      case loadResult of
        Failed    -> liftIO $ do
            hClose hLog
            Left <$> readFile tempLog
        Succeeded -> do
            setContext [IIDecl (simpleImportDecl $ moduleName pRELUDE), IIModule modName]
            Right <$> getSession

    df <- liftIO $ evalGhcEnv hscEnv' getSessionDynFlags
    let eval (stmt, l)
          | isStmt df stmt = do

            -- set up a custom interactive print function
            ctxt <- getContext
            setContext [IIDecl (simpleImportDecl $ moduleName pRELUDE)]
            let printFun = "let ghcideCustomShow x = Prelude.writeFile " <> show temp <> " (Prelude.show x)"
            interactivePrint <- execStmt printFun execOptions >>= \case
                ExecComplete (Right [interactivePrint]) _ -> pure interactivePrint
                _ -> error "internal error binding print function"
            modifySession $ \hsc -> hsc {hsc_IC = setInteractivePrintName (hsc_IC hsc) interactivePrint}
            setContext ctxt

            let opts =
                    execOptions
                    { execSourceFile = fp,
                        execLineNumber = l
                    }
            res <- execStmt stmt opts
            str <- case res of
                ExecComplete (Left err) _ -> pure $ pad $ show err
                ExecComplete (Right _) _ -> liftIO $ pad <$> readFile temp
                ExecBreak {} -> pure $ pad "breakpoints are not supported"

            let changes = [TextEdit editTarget $ T.pack str]
            return changes

          | isImport df stmt = do
              ctxt <- getContext
              idecl <- parseImportDecl stmt
              setContext $ IIDecl idecl : ctxt
              return []

          | otherwise = do
              void $ runDecls stmt
              return []

    edits <- liftIO $ evalGhcEnv hscEnv' $ traverse (eval . first T.unpack) statements

    let workspaceEditsMap = Map.fromList [(_uri, List $ concat edits)]
    let workspaceEdits = WorkspaceEdit (Just workspaceEditsMap) Nothing

    return (WorkspaceApplyEdit, ApplyWorkspaceEditParams workspaceEdits)

pad :: String -> String
pad = unlines . map ("-- " <>) . lines

-------------------------------------------------------------------------------

handleMaybe :: Monad m => e -> Maybe b -> ExceptT e m b
handleMaybe msg = maybe (throwE msg) return

handleMaybeM :: Monad m => e -> m (Maybe b) -> ExceptT e m b
handleMaybeM msg act = maybe (throwE msg) return =<< lift act

response :: ExceptT String IO a -> IO (Either ResponseError a)
response =
  fmap (first (\msg -> ResponseError InternalError (fromString msg) Nothing))
    . runExceptT

response' :: ExceptT String IO a -> IO (Either ResponseError Value, Maybe a)
response' act = do
  res <- runExceptT act
  case res of
    Left e ->
      return (Left (ResponseError InternalError (fromString e) Nothing), Nothing)
    Right a -> return (Right Null, Just a)

setupDynFlagsForGHCiLike :: HscEnv -> DynFlags -> IO DynFlags
setupDynFlagsForGHCiLike env dflags = do
  let dflags3 =
        dflags
          { hscTarget = HscInterpreted,
            ghcMode = CompManager,
            ghcLink = LinkInMemory
          }
      platform = targetPlatform dflags3
      dflags3a = updateWays $ dflags3 {ways = interpWays}
      dflags3b =
        foldl gopt_set dflags3a $
          concatMap
            (wayGeneralFlags platform)
            interpWays
      dflags3c =
        foldl gopt_unset dflags3b $
          concatMap
            (wayUnsetGeneralFlags platform)
            interpWays
      dflags4 =
        dflags3c `gopt_set` Opt_ImplicitImportQualified
          `gopt_set` Opt_IgnoreOptimChanges
          `gopt_set` Opt_IgnoreHpcChanges
  initializePlugins env dflags4

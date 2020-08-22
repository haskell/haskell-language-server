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
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.Class      (MonadTrans (lift))
import           Control.Monad.Trans.Except     (ExceptT (..), runExceptT,
                                                 throwE)
import           Data.Aeson                     (FromJSON, ToJSON, Value (Null),
                                                 toJSON)
import           Data.Bifunctor                 (Bifunctor (first))
import qualified Data.HashMap.Strict            as Map
import           Data.String                    (IsString (fromString))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Time                      (getCurrentTime)
import           Development.IDE.Core.Rules     (runAction)
import           Development.IDE.Core.RuleTypes (GetModSummary (..),
                                                 GhcSession (..))
import           Development.IDE.Core.Shake     (use_)
import           Development.IDE.GHC.Util       (evalGhcEnv, hscEnv,
                                                 textToStringBuffer)
import           Development.IDE.Types.Location (toNormalizedFilePath',
                                                 uriToFilePath')
import           DynamicLoading                 (initializePlugins)
import           DynFlags                       (targetPlatform)
import           GHC                            (DynFlags, ExecResult (..), GeneralFlag (Opt_IgnoreHpcChanges, Opt_IgnoreOptimChanges, Opt_ImplicitImportQualified),
                                                 GhcLink (LinkInMemory),
                                                 GhcMode (CompManager),
                                                 HscTarget (HscInterpreted),
                                                 LoadHowMuch (LoadAllTargets),
                                                 SuccessFlag (..),
                                                 execLineNumber, execOptions,
                                                 execSourceFile, execStmt,
                                                 getContext,
                                                 getInteractiveDynFlags,
                                                 getSession, getSessionDynFlags,
                                                 ghcLink, ghcMode, hscTarget,
                                                 isImport, isStmt, load,
                                                 moduleName, packageFlags,
                                                 parseImportDecl, pkgDatabase,
                                                 pkgState, runDecls, setContext,
                                                 setInteractiveDynFlags,
                                                 setLogAction,
                                                 setSessionDynFlags, setTargets,
                                                 simpleImportDecl, typeKind, ways)
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
import           Language.Haskell.LSP.VFS       (virtualFileText)
import           PrelNames                      (pRELUDE)
import           System.FilePath
import           System.IO                      (hClose)
import           System.IO.Temp
import Data.Maybe (catMaybes)
import qualified Control.Exception             as E
import           Control.DeepSeq                ( NFData
                                                , deepseq
                                                )
import Outputable (Outputable(ppr), showSDoc)
import Control.Applicative ((<|>))

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
      | Just l' <- T.stripPrefix "--" l =
        not (" >>>" `T.isPrefixOf` l')
      | otherwise =
        False

    goSearch _ [] = []
    goSearch line (l : ll)
      | Just match <- checkMatch l =
        goAcc (line + 1) [(match, line)] ll
      | otherwise =
        goSearch (line + 1) ll

    goAcc line acc [] = [(reverse acc, Range p p)] where p = Position line 0
    goAcc line acc (l : ll)
      | Just match <- checkMatch l =
        goAcc (line + 1) ([(match, line)] <> acc) ll
      | otherwise =
        (reverse acc, r) : goSearch (line + 1) ll
      where
        r = Range p p'
        p = Position line 0
        p' = Position (line + spliceLength) 0
        spliceLength = length (takeWhile looksLikeSplice (l : ll))

provider :: CodeLensProvider
provider lsp _state plId CodeLensParams {_textDocument} = response $ do
  let TextDocumentIdentifier uri = _textDocument
  contents <- liftIO $ getVirtualFileFunc lsp $ toNormalizedUri uri
  let text = virtualFileText <$> contents
  let matches = extractMatches text

  cmd <- liftIO $ mkLspCommand plId evalCommandName "Evaluate..." (Just [])

  let lenses =
        [ CodeLens range (Just cmd') Nothing
          | (m, r) <- matches,
            let (_, startLine) = head m
                (endLineContents, endLine) = last m
                range = Range start end
                start = Position startLine 0
                end = Position endLine (T.length endLineContents)
                args = EvalParams m r _textDocument,
            let cmd' =
                  (cmd :: Command)
                    { _arguments = Just (List [toJSON args]),
                      _title = if trivial r then "Evaluate..." else "Refresh..."
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
  text <- handleMaybe "contents" $ virtualFileText <$> contents

{- Note: GhcSessionDeps

Depending on GhcSession means we do need to reload all the module
dependencies in the GHC session(from interface files, hopefully).

The GhcSessionDeps dependency would allow us to reuse a GHC session preloaded
with all the dependencies. Unfortunately, the ModSummary objects that
GhcSessionDeps puts in the GHC session are not suitable for reuse since they
clear out the timestamps; this is done to avoid internal ghcide bugs and
can probably be relaxed so that plugins like Eval can reuse them. Once that's
done, we want to switch back to GhcSessionDeps:

-- https://github.com/digital-asset/ghcide/pull/694

 -}
  session <-
    liftIO $
      runAction "runEvalCmd.ghcSession" state $
        use_ GhcSession $ -- See the note on GhcSessionDeps
          toNormalizedFilePath' $
            fp

  ms <-
    liftIO $
      runAction "runEvalCmd.getModSummary" state $
        use_ GetModSummary $
          toNormalizedFilePath' $
            fp

  now <- liftIO getCurrentTime

  let tmp = withSystemTempFile (takeFileName fp)

  tmp $ \temp _h -> tmp $ \tempLog hLog -> do
    liftIO $ hClose _h
    let modName = moduleName $ ms_mod ms
        thisModuleTarget = Target (TargetFile fp Nothing) False (Just (textToStringBuffer text, now))

    hscEnv' <- ExceptT $
      evalGhcEnv (hscEnv session) $ do
        env <- getSession
        df <- liftIO $ setupDynFlagsForGHCiLike env $ ms_hspp_opts ms
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
          Failed -> liftIO $ do
            hClose hLog
            Left <$> readFile tempLog
          Succeeded -> do
            setContext [IIDecl (simpleImportDecl $ moduleName pRELUDE), IIModule modName]
            Right <$> getSession

    df <- liftIO $ evalGhcEnv hscEnv' getSessionDynFlags
    let eval (stmt, l)
          | let stmt0 = T.strip $ T.pack stmt -- For stripping and de-prefixing
          , Just (reduce, type_) <-
                  (True,) <$> T.stripPrefix ":kind! " stmt0
              <|> (False,) <$> T.stripPrefix ":kind " stmt0
          = do
            let input = T.strip type_
            (ty, kind) <- typeKind reduce $ T.unpack input
            pure $ Just
              $ T.unlines 
              $ map ("-- " <>)
              $ (input <> " :: " <> T.pack (showSDoc df $ ppr kind))
              : [ "= " <> T.pack (showSDoc df $ ppr ty) | reduce]
          | isStmt df stmt = do
            -- set up a custom interactive print function
            liftIO $ writeFile temp ""
            ctxt <- getContext
            setContext [IIDecl (simpleImportDecl $ moduleName pRELUDE)]
            let printFun = "let ghcideCustomShow x = Prelude.writeFile " <> show temp <> " (Prelude.show x)"
            interactivePrint <-
              execStmt printFun execOptions >>= \case
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
            case res of
              ExecComplete (Left err) _ -> return $ Just $ T.pack $ pad $ show err
              ExecComplete (Right _) _ -> do
                  out <- liftIO $ pad <$> readFile temp
                  -- Important to take the length in order to read the file eagerly
                  return $! if length out == 0 then Nothing else Just (T.pack out)
              ExecBreak {} -> return $ Just $ T.pack $ pad "breakpoints are not supported"

          | isImport df stmt = do
            ctxt <- getContext
            idecl <- parseImportDecl stmt
            setContext $ IIDecl idecl : ctxt
            return Nothing
          | otherwise = do
            void $ runDecls stmt
            return Nothing

    edits <-
      liftIO
      $ (either (\e -> [Just . T.pack . pad $ e]) id <$>)
      $ strictTry
      $ evalGhcEnv hscEnv'
      $ traverse (eval . first T.unpack) statements


    let workspaceEditsMap = Map.fromList [(_uri, List [evalEdit])]
        workspaceEdits = WorkspaceEdit (Just workspaceEditsMap) Nothing
        evalEdit = TextEdit editTarget (T.intercalate "\n" $ catMaybes edits)

    return (WorkspaceApplyEdit, ApplyWorkspaceEditParams workspaceEdits)

strictTry :: NFData b => IO b -> IO (Either String b)
strictTry op = E.catch
  (op >>= \v -> return $! Right $! deepseq v v)
  (\(err :: E.SomeException) -> return $! Left $ show err)

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

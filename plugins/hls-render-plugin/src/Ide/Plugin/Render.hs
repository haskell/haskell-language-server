{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeFamilies        #-}
module Ide.Plugin.Render (
    descriptor
) where

import           Development.IDE                              (IdeState,
                                                               LinkableType (BCOLinkable),
                                                               TcModuleResult (tmrTypechecked),
                                                               defineEarlyCutoff,
                                                               modifyDynFlags,
                                                               srcSpanToRange,
                                                               use_,
                                                               GetParsedModule (GetParsedModule),
                                                               RuleResult,
                                                               Rules,
                                                               VFSModified (VFSUnmodified),
                                                               evalGhcEnv,
                                                               runAction,
                                                               useNoFile_,
                                                               uses_)
import           Development.IDE.Core.PluginUtils             (runActionE,
                                                               uriToFilePathE,
                                                               useWithStaleE)
import           Ide.Logger                                   (Pretty (pretty),
                                                               Priority (Debug),
                                                               Recorder,
                                                               WithPriority,
                                                               cmapWithPrio,
                                                               logWith)
import           Ide.Plugin.Error                             (PluginError)
import           Ide.Types                                    (CommandFunction,
                                                               CommandId,
                                                               Config,
                                                               ConfigDescriptor (configCustomConfig),
                                                               HandlerM,
                                                               PluginCommand (PluginCommand),
                                                               PluginDescriptor (pluginCommands, pluginConfigDescriptor, pluginHandlers, pluginRules),
                                                               PluginId,
                                                               PluginMethodHandler,
                                                               defaultConfigDescriptor,
                                                               defaultPluginDescriptor,
                                                               mkCustomConfig,
                                                               mkLspCommand,
                                                               mkPluginHandler,
                                                               pluginWithIndefiniteProgress)
import           Language.LSP.Protocol.Lens                   (HasTextDocument (textDocument))
import           Language.LSP.Protocol.Message                (Method (Method_TextDocumentCodeLens),
                                                               SMethod (SMethod_TextDocumentCodeLens))
import           Language.LSP.Protocol.Types                  (CodeLens (CodeLens),
                                                               Command,
                                                               Null (Null),
                                                               TextDocumentIdentifier (..),
                                                               type (|?) (InL, InR))

import           Control.Monad.Trans.Except                   (ExceptT (..),
                                                               runExceptT)
import           Data.Function                                ((&))
import           Development.IDE.Core.RuleTypes               (GetLinkable (..),
                                                               GetModSummary (..),
                                                               GetModuleGraph (..),
                                                               GhcSessionDeps (..),
                                                               NeedsCompilation (..),
                                                               TypeCheck (TypeCheck),
                                                               encodeLinkableType,
                                                               linkableHomeMod,
                                                               msrModSummary)
import           Development.IDE.GHC.Compat.Core              (DynFlags (canUseColor, useColor),
                                                               GenLocated (..),
                                                               GenModule (moduleName),
                                                               GeneralFlag (Opt_DiagnosticsShowCaret, Opt_ImplicitImportQualified),
                                                               Ghc, GhcMonad,
                                                               GhcPs,
                                                               HasOccName (occName),
                                                               HasSrcSpan (..),
                                                               HomeModInfo (hm_iface),
                                                               HsDecl (SigD),
                                                               HsModule (hsmodDecls),
                                                               HscEnv,
                                                               InteractiveImport (IIDecl),
                                                               ModIface_ (mi_globals, mi_module),
                                                               ModSummary (ms_hspp_opts),
                                                               Name,
                                                               Sig (TypeSig),
                                                               SrcSpan,
                                                               TcRnExprMode (TM_Default),
                                                               forceGlobalRdrEnv,
                                                               getContext,
                                                               gopt_set,
                                                               gopt_unset,
                                                               ms_mod,
                                                               occNameString,
                                                               parseImportDecl,
                                                               runDecls,
                                                               setContext,
                                                               tcg_rdr_env,
                                                               unLoc, xopt_set,
                                                               xopt_unset)
import           Development.IDE.GHC.Compat.Env               (ghciBackend,
                                                               setBackend)
import           Development.IDE.Types.Location
import           GHC                                          (ParsedModule (pm_parsed_source),
                                                               SrcSpanAnnA)
--import GHC (unLoc, HsDecl (ValD), HsBindLR (..), GenLocated (L), HsExpr (HsVar), ParsedModule (pm_parsed_source), SrcSpanAnn' (SrcSpanAnn), GhcPs, SrcSpanAnnA, DynFlags (useColor, canUseColor), Target (Target), TargetId (TargetFile), load, LoadHowMuch (LoadAllTargets), guessTarget, Ghc)
import           Control.Exception.Base                       (bracket_)
import           Control.Exception.Safe                       (catchAny)
import           Control.Lens                                 ((^.))
import           Control.Monad                                (forM)
import           Control.Monad.IO.Class                       (liftIO)
import           Data.Aeson                                   (FromJSON, ToJSON,
                                                               toJSON)
import qualified Data.ByteString                              as BS
import           Data.Foldable                                (traverse_)
import           Data.HashSet                                 (HashSet)
import qualified Data.HashSet                                 as Set
import           Data.IORef                                   (IORef,
                                                               atomicModifyIORef',
                                                               newIORef,
                                                               readIORef)
import           Data.List                                    (isPrefixOf)
import           Data.Maybe                                   (mapMaybe)
import qualified Data.Text                                    as T
import           Development.IDE.Core.Compile                 (loadModulesHome)
import           Development.IDE.Core.FileStore               (addIdeGlobal,
                                                               setSomethingModified)
import           Development.IDE.Core.Rules                   (needsCompilationRule,
                                                               transitiveModuleDeps)
import           Development.IDE.Core.Shake                   (IsIdeGlobal,
                                                               RuleBody (RuleNoDiagnostics, RuleWithCustomNewnessCheck),
                                                               getIdeGlobalAction,
                                                               getIdeGlobalState)
import qualified Development.IDE.Core.Shake                   as Shake
import qualified Development.IDE.GHC.Compat                   as Compat
import           Development.IDE.GHC.Compat.Util              (OverridingBool (Never))
import           Development.IDE.Graph                        (alwaysRerun)
import           Development.IDE.Graph.Classes                (Hashable, NFData)
import           Development.IDE.Import.DependencyInformation (transitiveDeps)
import           Development.IDE.Types.HscEnvEq               (HscEnvEq (hscEnv))
import           Development.IDE.Types.Shake                  (toKey)
import           GHC.Driver.Ppr                               (showPprUnsafe)
import           GHC.Generics                                 (Generic)
import qualified GHC.LanguageExtensions.Type                  as LangExt (Extension (..))
import           GHC.Runtime.Eval                             (ExecOptions (..),
                                                               execOptions,
                                                               execStmt,
                                                               exprType)
import           Ide.Plugin.Render.Config                     (RenderConfig (..),
                                                               getRenderConfig,
                                                               properties)
import           Language.LSP.Server                          (ProgressCancellable (Cancellable))
import           UnliftIO                                     (MonadIO)

data RenderParams = RenderParams
  { paramValue      :: String
  , paramAction     :: String
  , paramModuleName :: !TextDocumentIdentifier
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Pretty RenderParams where
    pretty RenderParams {..} =
        "Render="
            <> pretty paramModuleName
            <> " : "
            <> pretty paramAction
            <> " "
            <> pretty paramValue

data Log
  = LogAvailableRenderActions [RenderParams]
  | LogRunningRenderAction RenderParams
  | LogShake Shake.Log

instance Pretty Log where
  pretty (LogAvailableRenderActions actions) = "Render : Available Actions : " <> pretty actions
  pretty (LogRunningRenderAction action) = "Render : Running : " <> pretty action
  pretty (LogShake s) = "Render : Shake :" <> pretty s

codeLensHandler :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState Method_TextDocumentCodeLens
codeLensHandler recorder st plId params = do
    rangeCommands <- mkRangeCommands recorder st plId (params ^. textDocument)
    pure
        $ InL
            [ CodeLens range (Just command) Nothing
            | (range, command) <- rangeCommands
            ]

mkRangeCommands :: Recorder (WithPriority Log) -> IdeState -> PluginId -> TextDocumentIdentifier -> ExceptT PluginError (HandlerM Config) [(Range, Command)]
mkRangeCommands recorder st plId textDocument =
    let dbg = logWith recorder Debug
     in do
                let TextDocumentIdentifier uri = textDocument
                fp <- uriToFilePathE uri
                let nfp = toNormalizedFilePath' fp
                (mod, _positioning) <- runActionE "render.GetParsedModule" st $ useWithStaleE GetParsedModule nfp
                let topLevelDecls :: [GenLocated SrcSpanAnnA (HsDecl GhcPs)]
                    topLevelDecls = mod
                      & pm_parsed_source
                      & unLoc
                      & hsmodDecls

                let getFunBind'' :: HasSrcSpan a => GenLocated a (HsDecl GhcPs) -> [(SrcSpan, String)]
                    getFunBind'' (L l (SigD _ (TypeSig _ [ids] _))) = [(getLoc l, occNameString . occName . unLoc $ ids)]
                    getFunBind'' _ = []

                let valueDecls = topLevelDecls >>= getFunBind''

                viableCommands <-
                  fmap (mconcat . mconcat) $
                    runInGhcEnv plId st fp $ \names _opts -> do
                      forM valueDecls $ \(l, v) ->
                        forM names $ \name -> do
                          let n = occNameString . occName $ name
                          (`catchAny` (const .  pure $ [])) $ do
                                t <- exprType TM_Default (n <> " " <> v)
                                return [(l,
                                           RenderParams
                                            { paramAction = n
                                            , paramValue =  v
                                            , paramModuleName = textDocument}
                                        ) | showPprUnsafe t == "IO ()"]

                let makeCommand (l, params@RenderParams {..}) =
                      let args = Just . pure . toJSON $ params
                          cmdText = "Render=" <> T.pack paramAction <> " " <> T.pack paramValue
                      in (, mkLspCommand plId renderCommandName cmdText args) <$> srcSpanToRange l
                let cmds = mapMaybe makeCommand viableCommands

                dbg . LogAvailableRenderActions $ (snd <$> viableCommands)
                pure cmds

renderCommandName :: CommandId
renderCommandName = "Render"

initialiseSessionForEval :: IdeState -> NormalizedFilePath -> IO HscEnv
initialiseSessionForEval st nfp = do
  (ms, env1) <- runAction "runRenderCmd" st $ do

    ms <- msrModSummary <$> use_ GetModSummary nfp
    deps_hsc <- hscEnv <$> use_ GhcSessionDeps nfp

    linkables_needed <- transitiveDeps <$> useNoFile_ GetModuleGraph <*> pure nfp
    linkables <- uses_ GetLinkable (nfp : maybe [] transitiveModuleDeps linkables_needed)
    -- We unset the global rdr env in mi_globals when we generate interfaces
    -- See Note [Clearing mi_globals after generating an iface]
    -- However, the eval plugin (setContext specifically) requires the rdr_env
    -- for the current module - so get it from the Typechecked Module and add
    -- it back to the iface for the current module.
    tm <- tmrTypechecked <$> use_ TypeCheck nfp
    let rdr_env = tcg_rdr_env tm
    let linkable_hsc = loadModulesHome (map (addRdrEnv . linkableHomeMod) linkables) deps_hsc
        addRdrEnv hmi
          | iface <- hm_iface hmi
          , ms_mod ms == mi_module iface
#if MIN_VERSION_ghc(9,11,0)
          = hmi { hm_iface = set_mi_top_env (Just $ IfaceTopEnv (forceGlobalRdrEnv (globalRdrEnvLocal rdr_env)) (mkIfaceImports $ tcg_import_decls tm)) iface}
#else
          = hmi { hm_iface = iface { mi_globals = Just $!
#if MIN_VERSION_ghc(9,8,0)
                    forceGlobalRdrEnv
#endif
                      rdr_env
                }}
#endif
          | otherwise = hmi

    return (ms, linkable_hsc)
  -- Bit awkward we need to use evalGhcEnv here but setContext requires to run
  -- in the Ghc monad
  env2 <- liftIO $ evalGhcEnv env1 $ do
            setContext [Compat.IIModule (moduleName (ms_mod ms))]
            let df = flip xopt_set    LangExt.ExtendedDefaultRules
                   . flip xopt_unset  LangExt.MonomorphismRestriction
                   . flip gopt_set    Opt_ImplicitImportQualified
                   . flip gopt_unset  Opt_DiagnosticsShowCaret
                   . setBackend ghciBackend
                   $ (ms_hspp_opts ms) {
                        useColor = Never
                      , canUseColor = False }
            modifyDynFlags (const df)

            Compat.getSession
  return env2

newtype RenderingVar = RenderingVar (IORef (HashSet NormalizedFilePath))
instance IsIdeGlobal RenderingVar

data IsRendering = IsRendering
    deriving (Eq, Show, Generic)
instance Hashable IsRendering
instance NFData   IsRendering

type instance RuleResult IsRendering = Bool

queueForEvaluation :: IdeState -> NormalizedFilePath -> IO ()
queueForEvaluation ide nfp = do
    RenderingVar var <- getIdeGlobalState ide
    atomicModifyIORef' var (\fs -> (Set.insert nfp fs, ()))

unqueueForEvaluation :: IdeState -> NormalizedFilePath -> IO ()
unqueueForEvaluation ide nfp = do
    RenderingVar var <- getIdeGlobalState ide
    -- remove the module from the Evaluating state, so that next time it won't evaluate to True
    atomicModifyIORef' var $ \fs -> (Set.delete nfp fs, ())

splitImportsAndDecls :: String -> ([String], String)
splitImportsAndDecls src = go [] (lines src)
  where
    go imports ("":rest) = go imports rest
    go imports remainder@(line:rest)
      | "import" `isPrefixOf` line = go (line:imports) rest
      | otherwise = (imports, unlines remainder)
    go imports [] = (imports, "")

addImport :: GhcMonad m => String -> m [InteractiveImport]
addImport i = do
    ctx <- getContext
    idecl <- parseImportDecl i
    setContext $ IIDecl idecl : ctx
    getContext

runInGhcEnv :: MonadIO m => PluginId -> IdeState -> FilePath -> ([Name] -> ExecOptions -> Ghc b) -> m b
runInGhcEnv  plId st fp ghcAction = liftIO $ do
        let nfp = toNormalizedFilePath' fp
        actionsFile <- liftIO $ runAction "Render: Config" st $ render_cfg_filepath <$> getRenderConfig plId
        (imports, decls) <- liftIO $ splitImportsAndDecls <$> readFile actionsFile

        -- enable codegen for the module which we need to evaluate.
        final_hscEnv <- liftIO $ bracket_
          (setSomethingModified VFSUnmodified st "Render" $ do
            queueForEvaluation st nfp
            return [toKey IsRendering nfp]
            )
          (setSomethingModified VFSUnmodified st "Render" $ do
            unqueueForEvaluation st nfp
            return [toKey IsRendering nfp]
            )
          (initialiseSessionForEval st nfp)
        let l = 0
        let opts = execOptions{execSourceFile = fp, execLineNumber = l}
        evalGhcEnv final_hscEnv $ do
          traverse_ addImport imports
          names <- runDecls decls
          ghcAction names opts

runRenderCmd :: Recorder (WithPriority Log) -> PluginId -> CommandFunction IdeState RenderParams
runRenderCmd recorder plId st mtoken param@RenderParams{..} =
  let action =
        do
          logWith recorder Debug (LogRunningRenderAction param)
          let TextDocumentIdentifier{_uri} = paramModuleName
          fp <- uriToFilePathE _uri
          _ <- runInGhcEnv plId st fp $ \_names opts -> do
                        execStmt (paramAction <> " " <> paramValue) opts
          pure $ InR Null
  in ExceptT $
      pluginWithIndefiniteProgress "Rendering" mtoken Cancellable $ const (runExceptT action)


renderCommand :: Recorder (WithPriority Log) -> PluginId -> PluginCommand IdeState
renderCommand recorder plId = PluginCommand renderCommandName "run action" (runRenderCmd recorder plId)

-- Redefine the NeedsCompilation rule to set the linkable type to Just _
-- whenever the module is being evaluated
-- This will ensure that the modules are loaded with linkables
-- and the interactive session won't try to compile them on the fly,
-- leading to much better performance of the evaluate code lens
redefinedNeedsCompilation :: Recorder (WithPriority Log) -> Rules ()
redefinedNeedsCompilation recorder = defineEarlyCutoff (cmapWithPrio LogShake recorder) $ RuleWithCustomNewnessCheck (<=) $ \NeedsCompilation f -> do
    isRendering <- use_ IsRendering f
    if isRendering then do
        let linkableType = BCOLinkable
            fp = encodeLinkableType $ Just linkableType
        pure (Just fp, Just (Just linkableType))
    else
        needsCompilationRule f

isRenderingRule :: Recorder (WithPriority Log) -> Rules ()
isRenderingRule recorder = defineEarlyCutoff (cmapWithPrio LogShake recorder) $ RuleNoDiagnostics $ \IsRendering f -> do
    alwaysRerun
    RenderingVar var <- getIdeGlobalAction
    b <- liftIO $ (f `Set.member`) <$> readIORef var
    return (Just (if b then BS.singleton 1 else BS.empty), Just b)

rules :: Recorder (WithPriority Log) -> Rules ()
rules recorder = do
    redefinedNeedsCompilation recorder
    isRenderingRule recorder
    addIdeGlobal . RenderingVar =<< liftIO(newIORef mempty)

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
    (defaultPluginDescriptor plId "Provides a mechanism to evaluate values using code lenses")
      { pluginHandlers = mconcat
            [ mkPluginHandler SMethod_TextDocumentCodeLens (codeLensHandler recorder)
            ]
      , pluginCommands = [renderCommand recorder plId]
      , pluginRules = rules recorder
      , pluginConfigDescriptor = defaultConfigDescriptor
                                   { configCustomConfig = mkCustomConfig properties
                                   }
      }

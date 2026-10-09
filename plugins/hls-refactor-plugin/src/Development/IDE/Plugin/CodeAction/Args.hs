module Development.IDE.Plugin.CodeAction.Args
  ( CodeActionTitle,
    CodeActionPreferred,
    GhcideCodeActionResult,
    GhcideCodeAction,
    mkGhcideCAPlugin,
    mkGhcideCAsPlugin,
    ToTextEdit (..),
    ToCodeAction (..),
    wrap,
    mkCA,
  )
where

import           Control.Concurrent.STM.Stats                 (readTVarIO)
import           Control.Monad.Except                         (ExceptT (..),
                                                               runExceptT)
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Data.Either                                  (fromRight,
                                                               partitionEithers)
import           Data.Functor                                 ((<&>))
import           Data.IORef.Extra
import qualified Data.Map                                     as Map
import           Data.Maybe                                   (fromMaybe,
                                                               maybeToList)
import qualified Data.Text                                    as T
import qualified Data.Text.Utf16.Rope.Mixed                   as Rope
import           Development.IDE                              hiding
                                                              (pluginHandlers)
import           Development.IDE.Core.PluginUtils             (activeDiagnosticsInRange)
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.ExactPrint
import           Development.IDE.Plugin.CodeAction.ExactPrint (Rewrite,
                                                               rewriteToEdit)
import           Development.IDE.Plugin.TypeLenses            (GetGlobalBindingTypeSigs (GetGlobalBindingTypeSigs),
                                                               GlobalBindingTypeSigsResult)
import           Development.IDE.Spans.LocalBindings          (Bindings)
import           Development.IDE.Types.Exports                (ExportsMap)
import           Development.IDE.Types.Options                (IdeOptions)
import           Ide.Plugin.Error                             (PluginError)
import           Ide.Types
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types

type CodeActionTitle = T.Text

type CodeActionPreferred = Bool

type GhcideCodeActionResult = [(CodeActionTitle, Maybe CodeActionKind, Maybe CodeActionPreferred, [TextEdit])]

type GhcideCodeAction = ExceptT PluginError (ReaderT CodeActionArgs IO) GhcideCodeActionResult

-------------------------------------------------------------------------------------------------

runGhcideCodeAction :: IdeState -> MessageParams Method_TextDocumentCodeAction -> GhcideCodeAction -> HandlerM Config GhcideCodeActionResult
runGhcideCodeAction state (CodeActionParams _ _ (TextDocumentIdentifier uri) _range _) codeAction
    | Just nfp <- toNormalizedFilePath' <$> uriToFilePath uri = do
        let runRule key = runAction ("GhcideCodeActions." <> show key) state $ runMaybeT $ MaybeT (pure (Just nfp)) >>= MaybeT . use key
        caaGhcSession <- onceIO $ runRule GhcSession
        caaExportsMap <-
            onceIO $
            caaGhcSession >>= \case
                Just env -> do
                    pkgExports <- envPackageExports env
                    localExports <- readTVarIO (exportsMap $ shakeExtras state)
                    pure $ localExports <> pkgExports
                _ -> pure mempty
        caaIdeOptions <- onceIO $ runAction "GhcideCodeActions.getIdeOptions" state getIdeOptions
        caaParsedModule <- onceIO $ runRule GetParsedModuleWithComments
        caaContents <-
            onceIO $
            runRule GetFileContents <&> \case
                Just (_, mbContents) -> fmap Rope.toText mbContents
                Nothing       -> Nothing
        caaDf <- onceIO $ fmap (ms_hspp_opts . pm_mod_summary) <$> caaParsedModule
        caaAnnSource <- onceIO $ runRule GetAnnotatedParsedSource
        caaTmr <- onceIO $ runRule TypeCheck
        caaHar <- onceIO $ runRule GetHieAst
        caaBindings <- onceIO $ runRule GetBindings
        caaGblSigs <- onceIO $ runRule GetGlobalBindingTypeSigs
        diags <- concat . maybeToList <$> activeDiagnosticsInRange (shakeExtras state) nfp _range
        results <- liftIO $
            sequence
                [
                    runReaderT (runExceptT codeAction) CodeActionArgs {..}
                        | caaDiagnostic <- diags
                ]
        let (_errs, successes) = partitionEithers results
        pure $ concat successes
    | otherwise = pure []


mkCA :: T.Text -> Maybe CodeActionKind -> Maybe Bool -> [Diagnostic] -> WorkspaceEdit -> (Command |? CodeAction)
mkCA title kind isPreferred diags edit =
  InR $ CodeAction title kind (Just diags) isPreferred Nothing (Just edit) Nothing Nothing

mkGhcideCAPlugin :: GhcideCodeAction -> PluginId -> T.Text -> PluginDescriptor IdeState
mkGhcideCAPlugin codeAction plId desc =
  (defaultPluginDescriptor plId desc)
    { pluginHandlers = mkPluginHandler SMethod_TextDocumentCodeAction $
        \state _ params@(CodeActionParams _ _ (TextDocumentIdentifier uri) _ CodeActionContext {_diagnostics = diags}) -> do
          results <- lift $ runGhcideCodeAction state params codeAction
          pure $
              InL
                [ mkCA title kind isPreferred diags edit
                  | (title, kind, isPreferred, tedit) <- results,
                    let edit = WorkspaceEdit (Just $ Map.singleton uri tedit) Nothing Nothing
                ]
    }

mkGhcideCAsPlugin :: [GhcideCodeAction] -> PluginId -> T.Text -> PluginDescriptor IdeState
mkGhcideCAsPlugin codeActions = mkGhcideCAPlugin $ mconcat codeActions

-------------------------------------------------------------------------------------------------

class ToTextEdit a where
  toTextEdit :: CodeActionArgs -> a -> IO [TextEdit]

instance ToTextEdit TextEdit where
  toTextEdit _ = pure . pure

instance ToTextEdit Rewrite where
  toTextEdit CodeActionArgs {..} rw = fmap (fromMaybe []) $
    runMaybeT $ do
      df <- MaybeT caaDf
      let r = rewriteToEdit df rw
      pure $ fromRight [] r

instance ToTextEdit a => ToTextEdit [a] where
  toTextEdit caa = foldMap (toTextEdit caa)

instance ToTextEdit a => ToTextEdit (Maybe a) where
  toTextEdit caa = maybe (pure []) (toTextEdit caa)

instance (ToTextEdit a, ToTextEdit b) => ToTextEdit (Either a b) where
  toTextEdit caa = either (toTextEdit caa) (toTextEdit caa)

-------------------------------------------------------------------------------------------------

data CodeActionArgs = CodeActionArgs
  { caaExportsMap   :: IO ExportsMap,
    caaGhcSession   :: IO (Maybe HscEnvEq),
    caaIdeOptions   :: IO IdeOptions,
    caaParsedModule :: IO (Maybe ParsedModule),
    caaContents     :: IO (Maybe T.Text),
    caaDf           :: IO (Maybe DynFlags),
    caaAnnSource    :: IO (Maybe ParsedSource),
    caaTmr          :: IO (Maybe TcModuleResult),
    caaHar          :: IO (Maybe HieAstResult),
    caaBindings     :: IO (Maybe Bindings),
    caaGblSigs      :: IO (Maybe GlobalBindingTypeSigsResult),
    caaDiagnostic   :: FileDiagnostic
  }

-- | There's no concurrency in each provider,
-- so we don't need to be thread-safe here
onceIO :: MonadIO m => IO a -> m (IO a)
onceIO io = do
  var <- liftIO $ newIORef Nothing
  pure $
    readIORef var >>= \case
      Just x -> pure x
      _      -> io >>= \x -> writeIORef' var (Just x) >> pure x

-------------------------------------------------------------------------------------------------

wrap :: (ToCodeAction a) => a -> GhcideCodeAction
wrap = toCodeAction

class ToCodeAction a where
  toCodeAction :: a -> GhcideCodeAction

instance ToCodeAction GhcideCodeAction where
  toCodeAction = id

instance Semigroup GhcideCodeAction where
  a <> b = toCodeAction [a, b]

instance Monoid GhcideCodeAction where
  mempty = pure []

instance ToCodeAction a => ToCodeAction [a] where
  toCodeAction = fmap concat . mapM toCodeAction

instance ToCodeAction a => ToCodeAction (Maybe a) where
  toCodeAction = maybe (pure []) toCodeAction

instance ToCodeAction a => ToCodeAction (Either PluginError a) where
  toCodeAction = either (\err -> ExceptT $ ReaderT $ \_ -> pure $ Left err) toCodeAction

instance ToTextEdit a => ToCodeAction (CodeActionTitle, a) where
  toCodeAction (title, te) = ExceptT $ ReaderT $ \caa -> Right . pure . (title,Just CodeActionKind_QuickFix,Nothing,) <$> toTextEdit caa te

instance ToTextEdit a => ToCodeAction (CodeActionTitle, CodeActionKind, a) where
  toCodeAction (title, kind, te) = ExceptT $ ReaderT $ \caa -> Right . pure . (title,Just kind,Nothing,) <$> toTextEdit caa te

instance ToTextEdit a => ToCodeAction (CodeActionTitle, CodeActionPreferred, a) where
  toCodeAction (title, isPreferred, te) = ExceptT $ ReaderT $ \caa -> Right . pure . (title,Just CodeActionKind_QuickFix,Just isPreferred,) <$> toTextEdit caa te

instance ToTextEdit a => ToCodeAction (CodeActionTitle, CodeActionKind, CodeActionPreferred, a) where
  toCodeAction (title, kind, isPreferred, te) = ExceptT $ ReaderT $ \caa -> Right . pure . (title,Just kind,Just isPreferred,) <$> toTextEdit caa te

-------------------------------------------------------------------------------------------------

toCodeAction1 :: (ToCodeAction r) => (CodeActionArgs -> IO (Maybe a)) -> (Maybe a -> r) -> GhcideCodeAction
toCodeAction1 get f = ExceptT . ReaderT $ \caa -> do
                          caaMay <- get caa
                          flip runReaderT caa . runExceptT . toCodeAction . f $ caaMay

toCodeAction2 :: (ToCodeAction r) => (CodeActionArgs -> IO (Maybe a)) -> (a -> r) -> GhcideCodeAction
toCodeAction2 get f = ExceptT . ReaderT $ \caa ->
  get caa >>= \case
    Just x -> flip runReaderT caa . runExceptT . toCodeAction . f $ x
    _      -> pure $ Right []

toCodeAction3 :: (ToCodeAction r) => (CodeActionArgs -> IO a) -> (a -> r) -> GhcideCodeAction
toCodeAction3 get f = ExceptT . ReaderT $ \caa -> get caa >>= flip runReaderT caa . runExceptT . toCodeAction . f

-- | this instance returns a delta AST, useful for exactprint transforms
instance ToCodeAction r => ToCodeAction (ParsedSource -> r) where
  toCodeAction = toCodeAction2 caaAnnSource

instance ToCodeAction r => ToCodeAction (ExportsMap -> r) where
  toCodeAction = toCodeAction3 caaExportsMap

instance ToCodeAction r => ToCodeAction (IdeOptions -> r) where
  toCodeAction = toCodeAction3 caaIdeOptions

instance ToCodeAction r => ToCodeAction (Diagnostic -> r) where
  toCodeAction f = ExceptT . ReaderT $ \caa@CodeActionArgs {caaDiagnostic = x} -> flip runReaderT caa . runExceptT . toCodeAction $ f (fdLspDiagnostic x)

instance ToCodeAction r => ToCodeAction (FileDiagnostic -> r) where
  toCodeAction f = ExceptT . ReaderT $ \caa@CodeActionArgs {caaDiagnostic = x} -> flip runReaderT caa . runExceptT . toCodeAction $ f x

instance ToCodeAction r => ToCodeAction (Maybe ParsedModule -> r) where
  toCodeAction = toCodeAction1 caaParsedModule

instance ToCodeAction r => ToCodeAction (ParsedModule -> r) where
  toCodeAction = toCodeAction2 caaParsedModule

instance ToCodeAction r => ToCodeAction (Maybe T.Text -> r) where
  toCodeAction = toCodeAction1 caaContents

instance ToCodeAction r => ToCodeAction (T.Text -> r) where
  toCodeAction = toCodeAction2 caaContents

instance ToCodeAction r => ToCodeAction (Maybe DynFlags -> r) where
  toCodeAction = toCodeAction1 caaDf

instance ToCodeAction r => ToCodeAction (DynFlags -> r) where
  toCodeAction = toCodeAction2 caaDf

instance ToCodeAction r => ToCodeAction (Maybe ParsedSource -> r) where
  toCodeAction = toCodeAction1 caaAnnSource

instance ToCodeAction r => ToCodeAction (Maybe TcModuleResult -> r) where
  toCodeAction = toCodeAction1 caaTmr

instance ToCodeAction r => ToCodeAction (TcModuleResult -> r) where
  toCodeAction = toCodeAction2 caaTmr

instance ToCodeAction r => ToCodeAction (Maybe HieAstResult -> r) where
  toCodeAction = toCodeAction1 caaHar

instance ToCodeAction r => ToCodeAction (HieAstResult -> r) where
  toCodeAction = toCodeAction2 caaHar

instance ToCodeAction r => ToCodeAction (Maybe Bindings -> r) where
  toCodeAction = toCodeAction1 caaBindings

instance ToCodeAction r => ToCodeAction (Bindings -> r) where
  toCodeAction = toCodeAction2 caaBindings

instance ToCodeAction r => ToCodeAction (Maybe GlobalBindingTypeSigsResult -> r) where
  toCodeAction = toCodeAction1 caaGblSigs

instance ToCodeAction r => ToCodeAction (GlobalBindingTypeSigsResult -> r) where
  toCodeAction = toCodeAction2 caaGblSigs

instance ToCodeAction r => ToCodeAction (Maybe HscEnvEq -> r) where
  toCodeAction = toCodeAction1 caaGhcSession

instance ToCodeAction r => ToCodeAction (Maybe HscEnv -> r) where
  toCodeAction = toCodeAction1 ((fmap.fmap.fmap) hscEnv caaGhcSession)

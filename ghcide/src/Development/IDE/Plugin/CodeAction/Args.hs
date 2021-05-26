{-# LANGUAGE FlexibleInstances #-}

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

import           Control.Concurrent.Extra
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Data.Either                                  (fromRight)
import qualified Data.HashMap.Strict                          as Map
import           Data.IORef.Extra
import           Data.Maybe                                   (fromMaybe)
import qualified Data.Text                                    as T
import           Development.IDE                              hiding
                                                              (pluginHandlers)
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
import           Ide.Plugin.Config                            (Config)
import           Ide.Types
import qualified Language.LSP.Server                          as LSP
import           Language.LSP.Types

type CodeActionTitle = T.Text

type CodeActionPreferred = Bool

type GhcideCodeActionResult = [(CodeActionTitle, Maybe CodeActionKind, Maybe CodeActionPreferred, [TextEdit])]

type GhcideCodeAction = ReaderT CodeActionArgs IO GhcideCodeActionResult

-------------------------------------------------------------------------------------------------

{-# ANN runGhcideCodeAction ("HLint: ignore Move guards forward" :: String) #-}
runGhcideCodeAction :: LSP.MonadLsp Config m => IdeState -> MessageParams TextDocumentCodeAction -> GhcideCodeAction -> m GhcideCodeActionResult
runGhcideCodeAction state (CodeActionParams _ _ (TextDocumentIdentifier uri) _range CodeActionContext {_diagnostics = List diags}) codeAction = do
  let mbFile = toNormalizedFilePath' <$> uriToFilePath uri
      runRule key = runAction ("GhcideCodeActions." <> show key) state $ runMaybeT $ MaybeT (pure mbFile) >>= MaybeT . use key
  caaExportsMap <-
    onceIO $
      runRule GhcSession >>= \case
        Just env -> do
          pkgExports <- envPackageExports env
          localExports <- readVar (exportsMap $ shakeExtras state)
          pure $ localExports <> pkgExports
        _ -> pure mempty
  caaIdeOptions <- onceIO $ runAction "GhcideCodeActions.getIdeOptions" state getIdeOptions
  caaParsedModule <- onceIO $ runRule GetParsedModule
  caaContents <-
    onceIO $
      runRule GetFileContents >>= \case
        Just (_, txt) -> pure txt
        _             -> pure Nothing
  caaDf <- onceIO $ fmap (ms_hspp_opts . pm_mod_summary) <$> caaParsedModule
  caaAnnSource <- onceIO $ runRule GetAnnotatedParsedSource
  caaTmr <- onceIO $ runRule TypeCheck
  caaHar <- onceIO $ runRule GetHieAst
  caaBindings <- onceIO $ runRule GetBindings
  caaGblSigs <- onceIO $ runRule GetGlobalBindingTypeSigs
  liftIO $
    concat
      <$> sequence
        [ runReaderT codeAction caa
          | caaDiagnostic <- diags,
            let caa = CodeActionArgs {..}
        ]

mkCA :: T.Text -> Maybe CodeActionKind -> Maybe Bool -> [Diagnostic] -> WorkspaceEdit -> (Command |? CodeAction)
mkCA title kind isPreferred diags edit =
  InR $ CodeAction title kind (Just $ List diags) isPreferred Nothing (Just edit) Nothing Nothing

mkGhcideCAPlugin :: GhcideCodeAction -> PluginId -> PluginDescriptor IdeState
mkGhcideCAPlugin codeAction plId =
  (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentCodeAction $
        \state _ params@(CodeActionParams _ _ (TextDocumentIdentifier uri) _ CodeActionContext {_diagnostics = List diags}) -> do
          results <- runGhcideCodeAction state params codeAction
          pure $
            Right $
              List
                [ mkCA title kind isPreferred diags edit
                  | (title, kind, isPreferred, tedit) <- results,
                    let edit = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing Nothing
                ]
    }

mkGhcideCAsPlugin :: [GhcideCodeAction] -> PluginId -> PluginDescriptor IdeState
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
      ps <- MaybeT caaAnnSource
      let r = rewriteToEdit df (annsA ps) rw
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
    caaIdeOptions   :: IO IdeOptions,
    caaParsedModule :: IO (Maybe ParsedModule),
    caaContents     :: IO (Maybe T.Text),
    caaDf           :: IO (Maybe DynFlags),
    caaAnnSource    :: IO (Maybe (Annotated ParsedSource)),
    caaTmr          :: IO (Maybe TcModuleResult),
    caaHar          :: IO (Maybe HieAstResult),
    caaBindings     :: IO (Maybe Bindings),
    caaGblSigs      :: IO (Maybe GlobalBindingTypeSigsResult),
    caaDiagnostic   :: Diagnostic
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

instance ToTextEdit a => ToCodeAction (CodeActionTitle, a) where
  toCodeAction (title, te) = ReaderT $ \caa -> pure . (title,Just CodeActionQuickFix,Nothing,) <$> toTextEdit caa te

instance ToTextEdit a => ToCodeAction (CodeActionTitle, CodeActionKind, a) where
  toCodeAction (title, kind, te) = ReaderT $ \caa -> pure . (title,Just kind,Nothing,) <$> toTextEdit caa te

instance ToTextEdit a => ToCodeAction (CodeActionTitle, CodeActionPreferred, a) where
  toCodeAction (title, isPreferred, te) = ReaderT $ \caa -> pure . (title,Just CodeActionQuickFix,Just isPreferred,) <$> toTextEdit caa te

instance ToTextEdit a => ToCodeAction (CodeActionTitle, CodeActionKind, CodeActionPreferred, a) where
  toCodeAction (title, kind, isPreferred, te) = ReaderT $ \caa -> pure . (title,Just kind,Just isPreferred,) <$> toTextEdit caa te

-------------------------------------------------------------------------------------------------

toCodeAction1 :: (ToCodeAction r) => (CodeActionArgs -> IO (Maybe a)) -> (Maybe a -> r) -> GhcideCodeAction
toCodeAction1 get f = ReaderT $ \caa -> get caa >>= flip runReaderT caa . toCodeAction . f

toCodeAction2 :: (ToCodeAction r) => (CodeActionArgs -> IO (Maybe a)) -> (a -> r) -> GhcideCodeAction
toCodeAction2 get f = ReaderT $ \caa ->
  get caa >>= \case
    Just x -> flip runReaderT caa . toCodeAction . f $ x
    _      -> pure []

toCodeAction3 :: (ToCodeAction r) => (CodeActionArgs -> IO a) -> (a -> r) -> GhcideCodeAction
toCodeAction3 get f = ReaderT $ \caa -> get caa >>= flip runReaderT caa . toCodeAction . f

instance ToCodeAction r => ToCodeAction (ParsedSource -> r) where
  toCodeAction f = ReaderT $ \caa@CodeActionArgs {caaAnnSource = x} ->
    x >>= \case
      Just s -> flip runReaderT caa . toCodeAction . f . astA $ s
      _      -> pure []

instance ToCodeAction r => ToCodeAction (ExportsMap -> r) where
  toCodeAction = toCodeAction3 caaExportsMap

instance ToCodeAction r => ToCodeAction (IdeOptions -> r) where
  toCodeAction = toCodeAction3 caaIdeOptions

instance ToCodeAction r => ToCodeAction (Diagnostic -> r) where
  toCodeAction f = ReaderT $ \caa@CodeActionArgs {caaDiagnostic = x} -> flip runReaderT caa . toCodeAction $ f x

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

instance ToCodeAction r => ToCodeAction (Maybe (Annotated ParsedSource) -> r) where
  toCodeAction = toCodeAction1 caaAnnSource

instance ToCodeAction r => ToCodeAction (Annotated ParsedSource -> r) where
  toCodeAction = toCodeAction2 caaAnnSource

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

{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies     #-}

-- | An HLS plugin to provide code lenses for type signatures
module Development.IDE.Plugin.NewCodeAction
  ( descriptor,
    suggestSignature,
    codeActionCommandId,
    NewCodeActionTypeSig (..),
    NewCodeActionTypeSigs (..),
    NewCodeActionTypeSigsResult (..),
  )
where

import           Control.Concurrent.STM.Stats        (atomically)
import           Control.DeepSeq                     (rwhnf)
import           Control.Monad                       (mzero)
import           Control.Monad.Extra                 (whenMaybe)
import           Control.Monad.IO.Class              (MonadIO (liftIO))
import           Data.Aeson.Types                    (Value (..), toJSON)
import qualified Data.Aeson.Types                    as A
import qualified Data.HashMap.Strict                 as Map
import           Data.List                           (find)
import           Data.Maybe                          (catMaybes, isJust)
import qualified Data.Text                           as T
import           Development.IDE                     (GhcSession (..),
                                                      HscEnvEq (hscEnv),
                                                      RuleResult, Rules, define,
                                                      srcSpanToRange,
                                                      unsafePrintSDoc)
import           Development.IDE.Core.Compile        (TcModuleResult (..))
import           Development.IDE.Core.RuleTypes      (GetBindings (GetBindings),
                                                      TypeCheck (TypeCheck))
import           Development.IDE.Core.Rules          (IdeState, runAction)
import           Development.IDE.Core.Service        (getDiagnostics)
import           Development.IDE.Core.Shake          (getHiddenDiagnostics,
                                                      ideLogger, logger, use)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error
import           Development.IDE.GHC.Util            (printName)
import           Development.IDE.Graph.Classes
import           Development.IDE.Spans.LocalBindings (Bindings, getFuzzyScope)
import           Development.IDE.Types.Location      (Position (Position, _character, _line),
                                                      Range (Range, _end, _start),
                                                      toNormalizedFilePath',
                                                      uriToFilePath')
import qualified Development.IDE.Types.Logger        as Logger
import           GHC.Generics                        (Generic)
import           Ide.Plugin.Config                   (Config)
import           Ide.Plugin.Properties
import           Ide.PluginUtils                     (mkLspCommand, subRange,
                                                      usePropertyLsp)
import           Ide.Types                           (CommandFunction,
                                                      CommandId (CommandId),
                                                      PluginCommand (PluginCommand),
                                                      PluginDescriptor (..),
                                                      PluginId,
                                                      configCustomConfig,
                                                      defaultConfigDescriptor,
                                                      defaultPluginDescriptor,
                                                      mkCustomConfig,
                                                      mkPluginHandler)
import qualified Language.LSP.Server                 as LSP
import           Language.LSP.Types                  (ApplyWorkspaceEditParams (ApplyWorkspaceEditParams),
                                                      CodeAction (..),
                                                      CodeActionKind (CodeActionQuickFix),
                                                      CodeActionParams (CodeActionParams, _range, _textDocument),
                                                      CodeLens (CodeLens),
                                                      CodeLensParams (CodeLensParams, _textDocument),
                                                      Command, Diagnostic (..),
                                                      List (..), ResponseError,
                                                      SMethod (..),
                                                      TextDocumentIdentifier (TextDocumentIdentifier),
                                                      TextEdit (TextEdit),
                                                      WorkspaceEdit (WorkspaceEdit),
                                                      type (|?) (InR))
import           Language.LSP.Types.Lens             (HasCodeActionProvider)
import           System.IO
import           Text.Regex.TDFA                     ((=~), (=~~))

codeActionCommandId :: T.Text
codeActionCommandId = "typesignature.change"

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
  (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionProvider,
      pluginCommands = [PluginCommand (CommandId codeActionCommandId) "change a signature" commandHandler],
      pluginRules = rules,
      pluginConfigDescriptor = defaultConfigDescriptor {configCustomConfig = mkCustomConfig properties}
    }

properties :: Properties '[ 'PropertyKey "mode" ('TEnum Mode)]
properties =
  emptyProperties
    & defineEnumProperty
      #mode
      "Control how type lenses are shown"
      [ (Always, "Always displays type lenses of global bindings"),
        (Exported, "Only display type lenses of exported global bindings"),
        (Diagnostics, "Follows error messages produced by GHC about missing signatures")
      ]
      Always

codeActionProvider :: LSP.MonadLsp Config m => IdeState
    -> PluginId
    -> CodeActionParams
    -> m (Either a (List (Command |? CodeAction)))
codeActionProvider ideState pId CodeActionParams {_textDocument = TextDocumentIdentifier uri, _range = currRange} = do
  mode <- usePropertyLsp #mode pId properties
  fmap (Right . List) $ case uriToFilePath' uri of
    Just (toNormalizedFilePath' -> filePath) -> liftIO $ do
      env <- fmap hscEnv <$> runAction "codeLens.GhcSession" ideState (use GhcSession filePath)
      tmr <- runAction "codeLens.TypeCheck" ideState (use TypeCheck filePath)
      bindings <- runAction "codeLens.GetBindings" ideState (use GetBindings filePath)
      -- gblSigs <- runAction "codeLens.NewCodeActionTypeSigs" ideState (use NewCodeActionTypeSigs filePath)

      diag <- atomically $ getDiagnostics ideState
      hDiag <- atomically $ getHiddenDiagnostics ideState
      -- liftIO $ Logger.logError (ideLogger ideState) $ T.pack $ show diag
      -- liftIO $ Logger.logError (ideLogger ideState) $ T.pack $ show hDiag

      let toWorkSpaceEdit tedit = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing Nothing
          matchDiags = filter (\(_, _, d) -> cantMatchType d && currRange `contains` d) (diag <> hDiag)
      liftIO $ Logger.logError (ideLogger ideState) $ T.pack $ show matchDiags
      hPrint stderr matchDiags
          -- generateActionForGlobal sig@NewCodeActionTypeSig {..} = do
          --   range <- srcSpanToRange $ gbSrcSpan sig
          --   tedit <- gblBindingTypeSigToEdit sig
          --   let wedit = toWorkSpaceEdit [tedit]
          --   pure $ generateAction pId range (T.pack gbRendered) wedit
          -- gblSigs' = maybe [] (\(NewCodeActionTypeSigsResult x) -> x) gblSigs
          -- generateActionFromDiags f =
          --   sequence
          --     [ pure $ generateAction pId _range title edit
          --       | (dFile, _, dDiag@Diagnostic {_range = _range}) <- diag ++ hDiag,
          --         dFile == filePath,
          --         (title, tedit) <- f dDiag,
          --         let edit = toWorkSpaceEdit tedit
          --     ]
      --pure (catMaybes $ generateActionForGlobal <$> gblSigs')
      pure []
    --   case mode of
    --     Always ->

    --         <> generateActionFromDiags (suggestLocalSignature False env tmr bindings) -- we still need diagnostics for local bindings
    --     Exported -> pure $ catMaybes $ generateActionForGlobal <$> filter gbExported gblSigs'
    --     Diagnostics -> generateActionFromDiags $ suggestSignature False env gblSigs tmr bindings
    -- Nothing -> pure []

generateAction :: p1 -> p2 -> T.Text -> WorkspaceEdit -> a |? CodeAction
generateAction _ _ title edit = InR CodeAction {
        _title = title,
        _kind = Just CodeActionQuickFix,
        _diagnostics = Nothing,
        _isPreferred = Nothing,
        _disabled = Nothing,
        _edit = Just edit,
        _command = Nothing,
        _xdata = Nothing
      }

commandHandler :: CommandFunction IdeState WorkspaceEdit
commandHandler _ideState wedit = do
  _ <- LSP.sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wedit) (\_ -> pure ())
  return $ Right Null

test :: Int -> Int
test x = length x

--------------------------------------------------------------------------------

suggestSignature :: Bool -> Maybe HscEnv -> Maybe NewCodeActionTypeSigsResult -> Maybe TcModuleResult -> Maybe Bindings -> Diagnostic -> [(T.Text, [TextEdit])]
suggestSignature isQuickFix env mGblSigs mTmr mBindings diag =
  suggestGlobalSignature isQuickFix mGblSigs diag <> suggestLocalSignature isQuickFix env mTmr mBindings diag

cantMatchType :: Diagnostic -> Bool
cantMatchType Diagnostic {_message} = _message =~ ("Couldn't match expected type" :: T.Text)

contains :: Range -> Diagnostic -> Bool
contains range Diagnostic {_range} = subRange range _range

suggestGlobalSignature :: Bool -> Maybe NewCodeActionTypeSigsResult -> Diagnostic -> [(T.Text, [TextEdit])]
suggestGlobalSignature isQuickFix mGblSigs Diagnostic {_message, _range}
  | _message
      =~ ("Couldn't match expected type" :: T.Text),
    Just (NewCodeActionTypeSigsResult sigs) <- mGblSigs,
    Just sig <- find (\x -> sameThing (gbSrcSpan x) _range) sigs,
    signature <- T.pack $ gbRendered sig,
    title <- if isQuickFix then "change signature: " <> signature else signature,
    Just action <- gblBindingTypeSigToEdit sig =
    [(title, [action])]
  | otherwise = []

suggestLocalSignature :: Bool -> Maybe HscEnv -> Maybe TcModuleResult -> Maybe Bindings -> Diagnostic -> [(T.Text, [TextEdit])]
suggestLocalSignature isQuickFix mEnv mTmr mBindings Diagnostic {_message, _range = _range@Range {..}}
  | Just (_ :: T.Text, _ :: T.Text, _ :: T.Text, [identifier]) <-
      (T.unwords . T.words $ _message)
        =~~ ("Polymorphic local binding with no type signature: (.*) ::" :: T.Text),
    Just bindings <- mBindings,
    Just env <- mEnv,
    localScope <- getFuzzyScope bindings _start _end,
    -- we can't use srcspan to lookup scoped bindings, because the error message reported by GHC includes the entire binding, instead of simply the name
    Just (name, ty) <- find (\(x, _) -> printName x == T.unpack identifier) localScope >>= \(name, mTy) -> (name,) <$> mTy,
    Just TcModuleResult {tmrTypechecked = TcGblEnv {tcg_rdr_env, tcg_sigs}} <- mTmr,
    -- not a top-level thing, to avoid duplication
    not $ name `elemNameSet` tcg_sigs,
    tyMsg <- printSDocQualifiedUnsafe (mkPrintUnqualifiedDefault env tcg_rdr_env) $ pprSigmaType ty,
    signature <- T.pack $ printName name <> " :: " <> tyMsg,
    startCharacter <- _character _start,
    startOfLine <- Position (_line _start) startCharacter,
    beforeLine <- Range startOfLine startOfLine,
    title <- if isQuickFix then "add signature: " <> signature else signature,
    action <- TextEdit beforeLine $ signature <> "\n" <> T.replicate (fromIntegral startCharacter) " " =
    [(title, [action])]
  | otherwise = []

sameThing :: SrcSpan -> Range -> Bool
sameThing s1 s2 = (_start <$> srcSpanToRange s1) == (_start <$> Just s2)

gblBindingTypeSigToEdit :: NewCodeActionTypeSig -> Maybe TextEdit
gblBindingTypeSigToEdit NewCodeActionTypeSig {..}
  | Just Range {..} <- srcSpanToRange $ getSrcSpan gbName,
    startOfLine <- Position (_line _start) 0,
    beforeLine <- Range startOfLine startOfLine =
    Just $ TextEdit beforeLine $ T.pack gbRendered <> "\n"
  | otherwise = Nothing

data Mode
  = -- | always displays type lenses of global bindings, no matter what GHC flags are set
    Always
  | -- | similar to 'Always', but only displays for exported global bindings
    Exported
  | -- |  follows error messages produced by GHC
    Diagnostics
  deriving (Eq, Ord, Show, Read, Enum)

instance A.ToJSON Mode where
  toJSON Always      = "always"
  toJSON Exported    = "exported"
  toJSON Diagnostics = "diagnostics"

instance A.FromJSON Mode where
  parseJSON = A.withText "Mode" $ \case
    "always"      -> pure Always
    "exported"    -> pure Exported
    "diagnostics" -> pure Diagnostics
    _             -> mzero

--------------------------------------------------------------------------------

showDocRdrEnv :: HscEnv -> GlobalRdrEnv -> SDoc -> String
showDocRdrEnv env rdrEnv = showSDocForUser' env (mkPrintUnqualifiedDefault env rdrEnv)

data NewCodeActionTypeSigs = NewCodeActionTypeSigs
  deriving (Generic, Show, Eq, Ord, Hashable, NFData)

data NewCodeActionTypeSig = NewCodeActionTypeSig
  { gbName     :: Name,
    gbRendered :: String,
    gbExported :: Bool
  }

gbSrcSpan :: NewCodeActionTypeSig -> SrcSpan
gbSrcSpan NewCodeActionTypeSig {gbName} = getSrcSpan gbName

newtype NewCodeActionTypeSigsResult = NewCodeActionTypeSigsResult [NewCodeActionTypeSig]

instance Show NewCodeActionTypeSigsResult where
  show _ = "<GetTypeResult>"

instance NFData NewCodeActionTypeSigsResult where
  rnf = rwhnf

type instance RuleResult NewCodeActionTypeSigs = NewCodeActionTypeSigsResult

rules :: Rules ()
rules = do
  define $ \NewCodeActionTypeSigs nfp -> do
    tmr <- use TypeCheck nfp
    -- we need session here for tidying types
    hsc <- use GhcSession nfp
    result <- liftIO $ gblBindingType (tmrRenamed <$> tmr) (hscEnv <$> hsc) (tmrTypechecked <$> tmr)
    pure ([], result)

gblBindingType :: Maybe RenamedSource -> Maybe HscEnv -> Maybe TcGblEnv -> IO (Maybe NewCodeActionTypeSigsResult)
gblBindingType (Just (rn, _, _, _)) (Just hsc) (Just gblEnv) = do
  let exports = availsToNameSet $ tcg_exports gblEnv
      -- TODO: check out `getTypeSigNames`
      -- sigs = tcg_sigs gblEnv
      sigs = maybe emptyNameSet getTypeSigNames $ case hs_valds rn of
          ValBinds _ _ s -> Just s
          _              -> Nothing
      binds = collectHsBindsBinders $ tcg_binds gblEnv
      patSyns = tcg_patsyns gblEnv
      showDoc = showDocRdrEnv hsc $ tcg_rdr_env gblEnv
  hPutStrLn stderr "Renamer stuff"
  hPutStrLn stderr $ unsafePrintSDoc $ ppr $ getTypeSigNames <$> (case hs_valds rn of
     ValBinds xvb bag gls -> Just gls
     XValBindsLR xvbl     -> Nothing)

  (_, maybe [] catMaybes -> bindings) <- initTcWithGbl hsc gblEnv (realSrcLocSpan $ mkRealSrcLoc "<dummy>" 1 1) $ mapM (bindingToSignature sigs exports showDoc) binds
  patterns <- catMaybes <$> mapM (patternToSignature sigs exports showDoc) patSyns
  pure . Just . NewCodeActionTypeSigsResult $ bindings <> patterns
gblBindingType _ _ _ = pure Nothing

-- convert a Binding into a Signature
bindingToSignature :: NameSet
    -> NameSet
    -> (SDoc -> String)
    -> Id
    -> IOEnv (Env TcGblEnv TcLclEnv) (Maybe NewCodeActionTypeSig)
bindingToSignature sigs exports showDoc id' = do
    let name = idName id'
    hasSignature name sigs $ do
          env <- tcInitTidyEnv
          let ty = tidyType env (idType id')
          pure $ NewCodeActionTypeSig name (printName name <> " :: " <> showDoc (pprSigmaType ty)) (name `elemNameSet` exports)

-- convert a Pattern into a Signature
patternToSignature :: Monad m =>
    NameSet
    -> NameSet
    -> (SDoc -> String)
    -> PatSyn
    -> m (Maybe NewCodeActionTypeSig)
patternToSignature sigs exports showDoc pat = do
    let name = patSynName pat
    hasSignature name sigs $ pure $ NewCodeActionTypeSig name ("pattern " <> printName name <> " :: " <> showDoc (pprPatSynTypeWithoutForalls pat)) (name `elemNameSet` exports)

-- | Top level bind that HAS a signature
hasSignature :: Monad m => Name -> NameSet -> m a -> m (Maybe a)
hasSignature name sigs = whenMaybe (not $ name `elemNameSet` sigs)

pprPatSynTypeWithoutForalls :: PatSyn -> SDoc
pprPatSynTypeWithoutForalls p = pprPatSynType pWithoutTypeVariables
  where
    pWithoutTypeVariables = mkPatSyn name declared_infix ([], req_theta) ([], prov_theta) orig_args' orig_res_ty matcher builder field_labels
    (_univ_tvs, req_theta, _ex_tvs, prov_theta, orig_args, orig_res_ty) = patSynSig p
    name = patSynName p
    declared_infix = patSynIsInfix p
    matcher = patSynMatcher p
    builder = patSynBuilder p
    field_labels = patSynFieldLabels p
    orig_args' = map scaledThing orig_args

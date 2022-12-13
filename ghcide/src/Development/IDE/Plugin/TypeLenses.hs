{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies     #-}

-- | An HLS plugin to provide code lenses for type signatures
module Development.IDE.Plugin.TypeLenses (
  descriptor,
  suggestSignature,
  typeLensCommandId,
  GlobalBindingTypeSig (..),
  GetGlobalBindingTypeSigs (..),
  GlobalBindingTypeSigsResult (..),
  Log(..)
  ) where

import           Control.Concurrent.STM.Stats        (atomically)
import           Control.DeepSeq                     (rwhnf)
import           Control.Monad                       (mzero)
import           Control.Monad.Extra                 (whenMaybe)
import           Control.Monad.IO.Class              (MonadIO (liftIO))
import           Data.Aeson.Types                    (Value (..), toJSON)
import qualified Data.Aeson.Types                    as A
import qualified Data.HashMap.Strict                 as Map
import           Data.List                           (find)
import           Data.Maybe                          (catMaybes)
import qualified Data.Text                           as T
import           Development.IDE                     (GhcSession (..),
                                                      HscEnvEq (hscEnv),
                                                      RuleResult, Rules, define,
                                                      srcSpanToRange,
                                                      usePropertyAction)
import           Development.IDE.Core.Compile        (TcModuleResult (..))
import           Development.IDE.Core.Rules          (IdeState, runAction)
import           Development.IDE.Core.RuleTypes      (GetBindings (GetBindings),
                                                      TypeCheck (TypeCheck))
import           Development.IDE.Core.Service        (getDiagnostics)
import           Development.IDE.Core.Shake          (getHiddenDiagnostics, use)
import qualified Development.IDE.Core.Shake          as Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Util            (printName)
import           Development.IDE.Graph.Classes
import           Development.IDE.Spans.LocalBindings (Bindings, getFuzzyScope)
import           Development.IDE.Types.Location      (Position (Position, _character, _line),
                                                      Range (Range, _end, _start),
                                                      toNormalizedFilePath',
                                                      uriToFilePath')
import           Development.IDE.Types.Logger        (Pretty (pretty), Recorder,
                                                      WithPriority,
                                                      cmapWithPrio)
import           GHC.Generics                        (Generic)
import           Ide.Plugin.Config                   (Config)
import           Ide.Plugin.Properties
import           Ide.PluginUtils                     (mkLspCommand)
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
                                                      CodeLens (CodeLens),
                                                      CodeLensParams (CodeLensParams, _textDocument),
                                                      Diagnostic (..),
                                                      List (..), ResponseError,
                                                      SMethod (..),
                                                      TextDocumentIdentifier (TextDocumentIdentifier),
                                                      TextEdit (TextEdit),
                                                      WorkspaceEdit (WorkspaceEdit))
import           Text.Regex.TDFA                     ((=~), (=~~))

data Log = LogShake Shake.Log deriving Show

instance Pretty Log where
  pretty = \case
    LogShake log -> pretty log

typeLensCommandId :: T.Text
typeLensCommandId = "typesignature.add"

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentCodeLens codeLensProvider
    , pluginCommands = [PluginCommand (CommandId typeLensCommandId) "adds a signature" commandHandler]
    , pluginRules = rules recorder
    , pluginConfigDescriptor = defaultConfigDescriptor {configCustomConfig = mkCustomConfig properties}
    }

properties :: Properties '[ 'PropertyKey "mode" ('TEnum Mode)]
properties = emptyProperties
  & defineEnumProperty #mode "Control how type lenses are shown"
    [ (Always, "Always displays type lenses of global bindings")
    , (Exported, "Only display type lenses of exported global bindings")
    , (Diagnostics, "Follows error messages produced by GHC about missing signatures")
    ] Always

codeLensProvider ::
  IdeState ->
  PluginId ->
  CodeLensParams ->
  LSP.LspM Config (Either ResponseError (List CodeLens))
codeLensProvider ideState pId CodeLensParams{_textDocument = TextDocumentIdentifier uri} = do
  mode <- liftIO $ runAction "codeLens.config" ideState $ usePropertyAction #mode pId properties
  fmap (Right . List) $ case uriToFilePath' uri of
    Just (toNormalizedFilePath' -> filePath) -> liftIO $ do
      env <- fmap hscEnv <$> runAction "codeLens.GhcSession" ideState (use GhcSession filePath)
      tmr <- runAction "codeLens.TypeCheck" ideState (use TypeCheck filePath)
      bindings <- runAction "codeLens.GetBindings" ideState (use GetBindings filePath)
      gblSigs <- runAction "codeLens.GetGlobalBindingTypeSigs" ideState (use GetGlobalBindingTypeSigs filePath)

      diag <- atomically $ getDiagnostics ideState
      hDiag <- atomically $ getHiddenDiagnostics ideState

      let toWorkSpaceEdit tedit = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing Nothing
          generateLensForGlobal sig@GlobalBindingTypeSig{..} = do
            range <- srcSpanToRange $ gbSrcSpan sig
            tedit <- gblBindingTypeSigToEdit sig
            let wedit = toWorkSpaceEdit [tedit]
            pure $ generateLens pId range (T.pack gbRendered) wedit
          gblSigs' = maybe [] (\(GlobalBindingTypeSigsResult x) -> x) gblSigs
          generateLensFromDiags f =
            sequence
              [ pure $ generateLens pId _range title edit
              | (dFile, _, dDiag@Diagnostic{_range = _range}) <- diag ++ hDiag
              , dFile == filePath
              , (title, tedit) <- f dDiag
              , let edit = toWorkSpaceEdit tedit
              ]

      case mode of
        Always ->
          pure (catMaybes $ generateLensForGlobal <$> gblSigs')
            <> generateLensFromDiags (suggestLocalSignature False env tmr bindings) -- we still need diagnostics for local bindings
        Exported -> pure $ catMaybes $ generateLensForGlobal <$> filter gbExported gblSigs'
        Diagnostics -> generateLensFromDiags $ suggestSignature False env gblSigs tmr bindings
    Nothing -> pure []

generateLens :: PluginId -> Range -> T.Text -> WorkspaceEdit -> CodeLens
generateLens pId _range title edit =
  let cId = mkLspCommand pId (CommandId typeLensCommandId) title (Just [toJSON edit])
   in CodeLens _range (Just cId) Nothing

commandHandler :: CommandFunction IdeState WorkspaceEdit
commandHandler _ideState wedit = do
  _ <- LSP.sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wedit) (\_ -> pure ())
  return $ Right Null

--------------------------------------------------------------------------------

suggestSignature :: Bool -> Maybe HscEnv -> Maybe GlobalBindingTypeSigsResult -> Maybe TcModuleResult -> Maybe Bindings -> Diagnostic -> [(T.Text, [TextEdit])]
suggestSignature isQuickFix env mGblSigs mTmr mBindings diag =
  suggestGlobalSignature isQuickFix mGblSigs diag <> suggestLocalSignature isQuickFix env mTmr mBindings diag

suggestGlobalSignature :: Bool -> Maybe GlobalBindingTypeSigsResult -> Diagnostic -> [(T.Text, [TextEdit])]
suggestGlobalSignature isQuickFix mGblSigs Diagnostic{_message, _range}
  | _message
      =~ ("(Top-level binding|Pattern synonym) with no type signature" :: T.Text)
    , Just (GlobalBindingTypeSigsResult sigs) <- mGblSigs
    , Just sig <- find (\x -> sameThing (gbSrcSpan x) _range) sigs
    , signature <- T.pack $ gbRendered sig
    , title <- if isQuickFix then "add signature: " <> signature else signature
    , Just action <- gblBindingTypeSigToEdit sig =
    [(title, [action])]
  | otherwise = []

suggestLocalSignature :: Bool -> Maybe HscEnv -> Maybe TcModuleResult -> Maybe Bindings -> Diagnostic -> [(T.Text, [TextEdit])]
suggestLocalSignature isQuickFix mEnv mTmr mBindings Diagnostic{_message, _range = _range@Range{..}}
  | Just (_ :: T.Text, _ :: T.Text, _ :: T.Text, [identifier]) <-
      (T.unwords . T.words $ _message)
        =~~ ("Polymorphic local binding with no type signature: (.*) ::" :: T.Text)
    , Just bindings <- mBindings
    , Just env <- mEnv
    , localScope <- getFuzzyScope bindings _start _end
    , -- we can't use srcspan to lookup scoped bindings, because the error message reported by GHC includes the entire binding, instead of simply the name
      Just (name, ty) <- find (\(x, _) -> printName x == T.unpack identifier) localScope >>= \(name, mTy) -> (name,) <$> mTy
    , Just TcModuleResult{tmrTypechecked = TcGblEnv{tcg_rdr_env, tcg_sigs}} <- mTmr
    , -- not a top-level thing, to avoid duplication
      not $ name `elemNameSet` tcg_sigs
    , tyMsg <- printSDocQualifiedUnsafe (mkPrintUnqualifiedDefault env tcg_rdr_env) $ pprSigmaType ty
    , signature <- T.pack $ printName name <> " :: " <> tyMsg
    , startCharacter <- _character _start
    , startOfLine <- Position (_line _start) startCharacter
    , beforeLine <- Range startOfLine startOfLine
    , title <- if isQuickFix then "add signature: " <> signature else signature
    , action <- TextEdit beforeLine $ signature <> "\n" <> T.replicate (fromIntegral startCharacter) " " =
    [(title, [action])]
  | otherwise = []

sameThing :: SrcSpan -> Range -> Bool
sameThing s1 s2 = (_start <$> srcSpanToRange s1) == (_start <$> Just s2)

gblBindingTypeSigToEdit :: GlobalBindingTypeSig -> Maybe TextEdit
gblBindingTypeSigToEdit GlobalBindingTypeSig{..}
  | Just Range{..} <- srcSpanToRange $ getSrcSpan gbName
    , startOfLine <- Position (_line _start) 0
    , beforeLine <- Range startOfLine startOfLine =
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

data GetGlobalBindingTypeSigs = GetGlobalBindingTypeSigs
  deriving (Generic, Show, Eq, Ord, Hashable, NFData)

data GlobalBindingTypeSig = GlobalBindingTypeSig
  { gbName     :: Name
  , gbRendered :: String
  , gbExported :: Bool
  }

gbSrcSpan :: GlobalBindingTypeSig -> SrcSpan
gbSrcSpan GlobalBindingTypeSig{gbName} = getSrcSpan gbName

newtype GlobalBindingTypeSigsResult = GlobalBindingTypeSigsResult [GlobalBindingTypeSig]

instance Show GlobalBindingTypeSigsResult where
  show _ = "<GetTypeResult>"

instance NFData GlobalBindingTypeSigsResult where
  rnf = rwhnf

type instance RuleResult GetGlobalBindingTypeSigs = GlobalBindingTypeSigsResult

rules :: Recorder (WithPriority Log) -> Rules ()
rules recorder = do
  define (cmapWithPrio LogShake recorder) $ \GetGlobalBindingTypeSigs nfp -> do
    tmr <- use TypeCheck nfp
    -- we need session here for tidying types
    hsc <- use GhcSession nfp
    result <- liftIO $ gblBindingType (hscEnv <$> hsc) (tmrTypechecked <$> tmr)
    pure ([], result)

gblBindingType :: Maybe HscEnv -> Maybe TcGblEnv -> IO (Maybe GlobalBindingTypeSigsResult)
gblBindingType (Just hsc) (Just gblEnv) = do
  let exports = availsToNameSet $ tcg_exports gblEnv
      sigs = tcg_sigs gblEnv
      binds = collectHsBindsBinders $ tcg_binds gblEnv
      patSyns = tcg_patsyns gblEnv
      rdrEnv = tcg_rdr_env gblEnv
      showDoc = showDocRdrEnv hsc rdrEnv
      hasSig :: (Monad m) => Name -> m a -> m (Maybe a)
      hasSig name f = whenMaybe (name `elemNameSet` sigs) f
      bindToSig id = do
        let name = idName id
        hasSig name $ do
          env <- tcInitTidyEnv
          let (_, ty) = tidyOpenType env (idType id)
          pure $ GlobalBindingTypeSig name (printName name <> " :: " <> showDoc (pprSigmaType ty)) (name `elemNameSet` exports)
      patToSig p = do
        let name = patSynName p
        hasSig name $ pure $ GlobalBindingTypeSig name ("pattern " <> printName name <> " :: " <> showDoc (pprPatSynTypeWithoutForalls p)) (name `elemNameSet` exports)
  (_, maybe [] catMaybes -> bindings) <- initTcWithGbl hsc gblEnv (realSrcLocSpan $ mkRealSrcLoc "<dummy>" 1 1) $ mapM bindToSig binds
  patterns <- catMaybes <$> mapM patToSig patSyns
  pure . Just . GlobalBindingTypeSigsResult $ bindings <> patterns
gblBindingType _ _ = pure Nothing

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

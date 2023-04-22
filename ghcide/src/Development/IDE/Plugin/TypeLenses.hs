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

import           Control.Concurrent.STM.Stats         (atomically)
import           Control.DeepSeq                      (rwhnf)
import           Control.Monad                        (mzero)
import           Control.Monad.Extra                  (whenMaybe)
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Data.Aeson.Types                     (Value (..), toJSON)
import qualified Data.Aeson.Types                     as A
import qualified Data.HashMap.Strict                  as Map
import           Data.List                            (find)
import           Data.Maybe                           (catMaybes, fromMaybe,
                                                       mapMaybe)
import qualified Data.Text                            as T
import           Development.IDE                      (GhcSession (..),
                                                       HscEnvEq (hscEnv),
                                                       RuleResult, Rules,
                                                       define, srcSpanToRange,
                                                       usePropertyAction,
                                                       useWithStale)
import           Development.IDE.Core.Compile         (TcModuleResult (..))
import           Development.IDE.Core.PositionMapping (PositionMapping,
                                                       toCurrentRange)
import           Development.IDE.Core.Rules           (IdeState, runAction)
import           Development.IDE.Core.RuleTypes       (GetBindings (GetBindings),
                                                       TypeCheck (TypeCheck))
import           Development.IDE.Core.Service         (getDiagnostics)
import           Development.IDE.Core.Shake           (getHiddenDiagnostics,
                                                       use)
import qualified Development.IDE.Core.Shake           as Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Util             (printName)
import           Development.IDE.Graph.Classes
import           Development.IDE.Spans.LocalBindings  (Bindings, getFuzzyScope)
import           Development.IDE.Types.Location       (Position (Position, _character, _line),
                                                       Range (Range, _end, _start))
import           Development.IDE.Types.Logger         (Pretty (pretty),
                                                       Recorder, WithPriority,
                                                       cmapWithPrio)
import           GHC.Generics                         (Generic)
import           Ide.Plugin.Properties
import           Ide.PluginUtils
import           Ide.Types                            (CommandFunction,
                                                       CommandId (CommandId),
                                                       PluginCommand (PluginCommand),
                                                       PluginDescriptor (..),
                                                       PluginId,
                                                       PluginMethodHandler,
                                                       configCustomConfig,
                                                       defaultConfigDescriptor,
                                                       defaultPluginDescriptor,
                                                       mkCustomConfig,
                                                       mkPluginHandler)
import qualified Language.LSP.Server                  as LSP
import           Language.LSP.Types                   (ApplyWorkspaceEditParams (ApplyWorkspaceEditParams),
                                                       CodeLens (CodeLens),
                                                       CodeLensParams (CodeLensParams, _textDocument),
                                                       Diagnostic (..),
                                                       List (..),
                                                       Method (TextDocumentCodeLens),
                                                       SMethod (..),
                                                       TextDocumentIdentifier (TextDocumentIdentifier),
                                                       TextEdit (TextEdit),
                                                       WorkspaceEdit (WorkspaceEdit))
import           Text.Regex.TDFA                      ((=~), (=~~))

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

codeLensProvider :: PluginMethodHandler IdeState TextDocumentCodeLens
codeLensProvider ideState pId CodeLensParams{_textDocument = TextDocumentIdentifier uri} = pluginResponse $ do
    mode <- liftIO $ runAction "codeLens.config" ideState $ usePropertyAction #mode pId properties
    nfp <- getNormalizedFilePath uri
    env <- hscEnv . fst
            <$> (handleMaybeM "Unable to get GhcSession"
                $ liftIO
                $ runAction "codeLens.GhcSession" ideState (useWithStale GhcSession nfp)
                )
    tmr <- fst <$> (
                handleMaybeM "Unable to TypeCheck"
              $ liftIO
              $ runAction "codeLens.TypeCheck" ideState (useWithStale TypeCheck nfp)
              )
    (bindings, bindingsMp) <-
      handleMaybeM "Unable to GetBindings"
      $ liftIO
      $ runAction "codeLens.GetBindings" ideState (useWithStale GetBindings nfp)
    (gblSigs@(GlobalBindingTypeSigsResult gblSigs'), gblSigsMp) <-
      handleMaybeM "Unable to GetGlobalBindingTypeSigs"
      $ liftIO
      $ runAction "codeLens.GetGlobalBindingTypeSigs" ideState (useWithStale GetGlobalBindingTypeSigs nfp)

    diag <- liftIO $ atomically $ getDiagnostics ideState
    hDiag <- liftIO $ atomically $ getHiddenDiagnostics ideState

    let toWorkSpaceEdit tedit = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing Nothing
        generateLensForGlobal mp sig@GlobalBindingTypeSig{gbRendered} = do
            range <- toCurrentRange mp =<< srcSpanToRange (gbSrcSpan sig)
            tedit <- gblBindingTypeSigToEdit sig (Just gblSigsMp)
            let wedit = toWorkSpaceEdit [tedit]
            pure $ generateLens pId range (T.pack gbRendered) wedit
        generateLensFromDiags f =
              [ generateLens pId _range title edit
              | (dFile, _, dDiag@Diagnostic{_range = _range}) <- diag ++ hDiag
              , dFile == nfp
              , (title, tedit) <- f dDiag
              , let edit = toWorkSpaceEdit tedit
              ]
    pure $ List $ case mode of
        Always ->
          mapMaybe (generateLensForGlobal gblSigsMp) gblSigs'
            <> generateLensFromDiags
                (suggestLocalSignature False (Just env) (Just tmr) (Just bindings) (Just bindingsMp)) -- we still need diagnostics for local bindings
        Exported -> mapMaybe (generateLensForGlobal gblSigsMp) (filter gbExported gblSigs')
        Diagnostics -> generateLensFromDiags
            $ suggestSignature' False (Just env) (Just gblSigs) (Just tmr) (Just bindings) (Just gblSigsMp) (Just bindingsMp)

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
suggestSignature isQuickFix env mGblSigs mTmr mBindings diag = suggestSignature' isQuickFix env mGblSigs mTmr mBindings Nothing Nothing diag

suggestSignature' ::
    Bool
    -> Maybe HscEnv
    -> Maybe GlobalBindingTypeSigsResult
    -> Maybe TcModuleResult
    -> Maybe Bindings
    -> Maybe PositionMapping
    -> Maybe PositionMapping
    -> Diagnostic
    -> [(T.Text, [TextEdit])]
suggestSignature' isQuickFix env mGblSigs mTmr mBindings gblMp bindingMp diag =
  suggestGlobalSignature isQuickFix mGblSigs gblMp diag <> suggestLocalSignature isQuickFix env mTmr mBindings bindingMp diag

suggestGlobalSignature :: Bool -> Maybe GlobalBindingTypeSigsResult -> Maybe PositionMapping -> Diagnostic -> [(T.Text, [TextEdit])]
suggestGlobalSignature isQuickFix mGblSigs mmp Diagnostic{_message, _range}
  | _message
      =~ ("(Top-level binding|Pattern synonym) with no type signature" :: T.Text)
    , Just (GlobalBindingTypeSigsResult sigs) <- mGblSigs
    , Just sig <- find (\x -> sameThing (gbSrcSpan x) _range) sigs
    , signature <- T.pack $ gbRendered sig
    , title <- if isQuickFix then "add signature: " <> signature else signature
    , Just action <- gblBindingTypeSigToEdit sig mmp =
    [(title, [action])]
  | otherwise = []

suggestLocalSignature :: Bool -> Maybe HscEnv -> Maybe TcModuleResult -> Maybe Bindings -> Maybe PositionMapping -> Diagnostic -> [(T.Text, [TextEdit])]
suggestLocalSignature isQuickFix mEnv mTmr mBindings mmp Diagnostic{_message, _range = _range@Range{..}}
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
    , range' <- fromMaybe beforeLine (flip toCurrentRange beforeLine =<< mmp)
    , action <- TextEdit range' $ signature <> "\n" <> T.replicate (fromIntegral startCharacter) " " =
    [(title, [action])]
  | otherwise = []

sameThing :: SrcSpan -> Range -> Bool
sameThing s1 s2 = (_start <$> srcSpanToRange s1) == (_start <$> Just s2)

gblBindingTypeSigToEdit :: GlobalBindingTypeSig -> Maybe PositionMapping -> Maybe TextEdit
gblBindingTypeSigToEdit GlobalBindingTypeSig{..} mmp
  | Just Range{..} <- srcSpanToRange $ getSrcSpan gbName
    , startOfLine <- Position (_line _start) 0
    , beforeLine <- Range startOfLine startOfLine
    , range' <- fromMaybe beforeLine (flip toCurrentRange beforeLine =<< mmp)
    = Just $ TextEdit range' $ T.pack gbRendered <> "\n"
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

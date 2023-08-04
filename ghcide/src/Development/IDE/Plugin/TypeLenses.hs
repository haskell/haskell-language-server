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
import           Control.Lens                         ((?~))
import           Control.Monad                        (mzero)
import           Control.Monad.Extra                  (whenMaybe)
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Class            (MonadTrans (lift))
import           Data.Aeson.Types                     (toJSON)
import qualified Data.Aeson.Types                     as A
import           Data.List                            (find, intercalate)
import qualified Data.Map                             as Map
import           Data.Maybe                           (catMaybes, fromMaybe,
                                                       maybeToList)
import qualified Data.Text                            as T
import           Development.IDE                      (GhcSession (..),
                                                       HscEnvEq (hscEnv),
                                                       RuleResult, Rules, Uri,
                                                       define, srcSpanToRange,
                                                       usePropertyAction)
import           Development.IDE.Core.Compile         (TcModuleResult (..))
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping (PositionMapping,
                                                       fromCurrentRange,
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
import           GHC.Generics                         (Generic)
import           Ide.Logger                           (Pretty (pretty),
                                                       Recorder, WithPriority,
                                                       cmapWithPrio)
import           Ide.Plugin.Error
import           Ide.Plugin.Properties
import           Ide.PluginUtils                      (mkLspCommand)
import           Ide.Types                            (CommandFunction,
                                                       CommandId (CommandId),
                                                       PluginCommand (PluginCommand),
                                                       PluginDescriptor (..),
                                                       PluginId,
                                                       PluginMethodHandler,
                                                       ResolveFunction,
                                                       configCustomConfig,
                                                       defaultConfigDescriptor,
                                                       defaultPluginDescriptor,
                                                       mkCustomConfig,
                                                       mkPluginHandler,
                                                       mkResolveHandler)
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Message        (Method (Method_CodeLensResolve, Method_TextDocumentCodeLens),
                                                       SMethod (..))
import           Language.LSP.Protocol.Types          (ApplyWorkspaceEditParams (ApplyWorkspaceEditParams),
                                                       CodeLens (..),
                                                       CodeLensParams (CodeLensParams, _textDocument),
                                                       Command, Diagnostic (..),
                                                       Null (Null),
                                                       TextDocumentIdentifier (TextDocumentIdentifier),
                                                       TextEdit (TextEdit),
                                                       WorkspaceEdit (WorkspaceEdit),
                                                       type (|?) (..))
import qualified Language.LSP.Server                  as LSP
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
    { pluginHandlers = mkPluginHandler SMethod_TextDocumentCodeLens codeLensProvider
                    <> mkResolveHandler SMethod_CodeLensResolve codeLensResolveProvider
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

codeLensProvider :: PluginMethodHandler IdeState Method_TextDocumentCodeLens
codeLensProvider ideState pId CodeLensParams{_textDocument = TextDocumentIdentifier uri} = do
    mode <- liftIO $ runAction "codeLens.config" ideState $ usePropertyAction #mode pId properties
    nfp <- getNormalizedFilePathE uri
    -- We have three ways we can possibly generate code lenses for type lenses.
    -- Different options are with different "modes" of the typelens plugin.
    -- (Remember here, as the code lens is not resolved yet, we only really need
    -- the range and any data that will help us resolve it later)
    let -- The first option is to generate the lens from diagnostics about local
        -- bindings.
        -- TODO: We need the identifier, but not sure we need the _range.
        -- One I get it to reliably work I can find out.
        generateLensFromLocalDiags diags =
          [ CodeLens _range Nothing (Just $ toJSON $ TypeLensesResolveLocal identifier _range)
             | (dFile, _, Diagnostic{_range, _message}) <- diags
            , dFile == nfp
            , Just (_ :: T.Text, _ :: T.Text, _ :: T.Text, [identifier]) <-
                [(T.unwords . T.words $ _message)
                  =~~ ("Polymorphic local binding with no type signature: (.*) ::" :: T.Text)]]
        -- The second option is to generate lens from diagnostics about
        -- top level bindings. Even though we don't need any extra data besides
        -- the range to resolve this later, we still need to put data in here
        -- because code lenses without data are not resolvable with HLS
        generateLensFromGlobalDiags diags =
          -- We have different methods for generating global lenses depending on
          -- the mode chosen, but all lenses are resolved the same way.
          [ CodeLens _range Nothing (Just $ toJSON TypeLensesResolveGlobal)
            | (dFile, _, Diagnostic{_range, _message}) <- diags
            , dFile == nfp
            , _message
                 =~ ("(Top-level binding|Pattern synonym) with no type signature" :: T.Text)]
        -- The third option is to generate lenses from the GlobalBindingTypeSig
        -- rule. This is the only type that needs to have the range adjusted
        -- with PositionMapping
        generateLensFromGlobal sigs mp = do
          [ CodeLens newRange Nothing (Just $ toJSON TypeLensesResolveGlobal)
            | sig <- sigs
            , Just range <- [srcSpanToRange (gbSrcSpan sig)]
            , Just newRange <- [toCurrentRange mp range]]
    case mode of
      Always -> do
        -- This is sort of a hybrid method, where we get the global bindings
        -- from the GlobalBindingTypeSigs rule, and the local bindings from
        -- diagnostics.
        diags <- liftIO $ atomically $ getDiagnostics ideState
        hDiags <- liftIO $ atomically $ getHiddenDiagnostics ideState
        (GlobalBindingTypeSigsResult gblSigs, gblSigsMp) <-
          runActionE "codeLens.GetGlobalBindingTypeSigs" ideState
          $ useWithStaleE GetGlobalBindingTypeSigs nfp
        pure $ InL $ generateLensFromGlobal gblSigs gblSigsMp
          <> generateLensFromLocalDiags (diags <> hDiags) -- we still need diagnostics for local bindings
      Exported -> do
        -- In this rule we only get bindings from the GlobalBindingTypeSigs
        -- rule, and in addition we filter out the non exported symbols
        (GlobalBindingTypeSigsResult gblSigs, gblSigsMp) <-
          runActionE "codeLens.GetGlobalBindingTypeSigs" ideState
          $ useWithStaleE GetGlobalBindingTypeSigs nfp
        pure $ InL $ generateLensFromGlobal (filter gbExported gblSigs) gblSigsMp
      Diagnostics -> do
        -- For this mode we exclusively use diagnostics to create the lenses.
        -- However we will still use the GlobalBindingTypeSigs to resolve them.
        -- This is how it was done also before the changes to support resolve.
        diags <- liftIO $ atomically $ getDiagnostics ideState
        hDiags <- liftIO $ atomically $ getHiddenDiagnostics ideState
        let allDiags = diags <> hDiags
        pure $ InL $ generateLensFromLocalDiags allDiags <> generateLensFromGlobalDiags allDiags

-- When resolving a type lens we only care whether it is local or global.
codeLensResolveProvider :: ResolveFunction IdeState TypeLensesResolveData Method_CodeLensResolve
codeLensResolveProvider ideState pId lens uri TypeLensesResolveLocal{identifier, range} = do
  nfp <- getNormalizedFilePathE uri
  (hscEnv -> env, _) <- runActionE "codeLens.GhcSession" ideState
      (useWithStaleE GhcSession nfp)
  (tmr, _) <- runActionE "codeLens.TypeCheck" ideState
    (useWithStaleE TypeCheck nfp)
  (bindings, _) <- runActionE "codeLens.GetBindings" ideState
    (useWithStaleE GetBindings nfp)
  -- To create a local signature, we need a lot more moving parts, as we don't
  -- have any specific rule created for it.
  (title, edit) <- handleMaybe PluginStaleResolve $ suggestLocalSignature' False (Just env) (Just tmr) (Just bindings) identifier range
  pure $ lens & L.command ?~ generateLensCommand pId uri title edit
codeLensResolveProvider ideState pId lens@CodeLens{_range} uri TypeLensesResolveGlobal = do
  nfp <- getNormalizedFilePathE uri
  (gblSigs@(GlobalBindingTypeSigsResult _), gblSigsMp) <-
    runActionE "codeLens.GetGlobalBindingTypeSigs" ideState
    $ useWithStaleE GetGlobalBindingTypeSigs nfp
  -- Resolving a global signature is by comparison much easier, as we have a
  -- specific rule just for that.
  (title, edit) <- handleMaybe PluginStaleResolve $ suggestGlobalSignature' False (Just gblSigs) (Just gblSigsMp) _range
  pure $ lens & L.command ?~ generateLensCommand pId uri title edit

generateLensCommand :: PluginId -> Uri -> T.Text -> TextEdit -> Command
generateLensCommand pId uri title edit =
  let wEdit = WorkspaceEdit (Just $ Map.singleton uri $ [edit]) Nothing Nothing
  in mkLspCommand pId (CommandId typeLensCommandId) title (Just [toJSON wEdit])

-- Since the lenses are created with diagnostics, and since the globalTypeSig
-- rule can't be changed as it is also used by the hls-refactor plugin, we can't
-- rely on actions. Because we can't rely on actions it doesn't make sense to
-- recompute the edit upon command. Hence the command here just takes a edit
-- and applies it.
commandHandler :: CommandFunction IdeState WorkspaceEdit
commandHandler _ideState wedit = do
  _ <- lift $ LSP.sendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wedit) (\_ -> pure ())
  pure $ InR Null

--------------------------------------------------------------------------------
-- To give one an idea about how creative hls-refactor plugin is, the end type
-- here can be changed within certain parameters, and even though it is used by
-- the hls-refactor-plugin, the hls-refactor-plugin itself won't need adaptions
suggestSignature :: Bool -> Maybe HscEnv -> Maybe GlobalBindingTypeSigsResult -> Maybe TcModuleResult -> Maybe Bindings -> Diagnostic -> [(T.Text, TextEdit)]
suggestSignature isQuickFix env mGblSigs mTmr mBindings diag =
  maybeToList (suggestGlobalSignature isQuickFix mGblSigs diag) <> maybeToList (suggestLocalSignature isQuickFix env mTmr mBindings diag)

-- Both the suggestGlobalSignature and suggestLocalSignature functions have been
-- broken up. The main functions works with a diagnostic, which then calls the
-- secondary function with whatever pieces of the diagnostic it needs. This
-- allows the resolve function, which no longer has the Diagnostic, to still
-- call the secondary functions.
suggestGlobalSignature :: Bool -> Maybe GlobalBindingTypeSigsResult -> Diagnostic -> Maybe (T.Text, TextEdit)
suggestGlobalSignature isQuickFix mGblSigs Diagnostic{_message, _range}
  | _message =~ ("(Top-level binding|Pattern synonym) with no type signature" :: T.Text) =
    suggestGlobalSignature' isQuickFix mGblSigs Nothing _range
  | otherwise = Nothing

-- In addition, for suggestGlobalSignature, we added the option of having a
-- PositionMapping. In this case if there is no PositionMapping provided, it will
-- ignore it. However if a PositionMapping is supplied, it will assume that the
-- range provided is already converted with the PositionMapping, and will attempt
-- to convert it back before attempting to find the signature from the rule.
suggestGlobalSignature' :: Bool -> Maybe GlobalBindingTypeSigsResult -> Maybe PositionMapping -> Range -> Maybe (T.Text, TextEdit)
suggestGlobalSignature' isQuickFix mGblSigs pm range
  |   Just (GlobalBindingTypeSigsResult sigs) <- mGblSigs
    , let newRange = fromMaybe range (pm >>= \x -> fromCurrentRange x range)
    , Just sig <- find (\x -> sameThing (gbSrcSpan x) newRange) sigs
    , signature <- T.pack $ gbRendered sig
    , title <- if isQuickFix then "add signature: " <> signature else signature
    , Just action <- gblBindingTypeSigToEdit sig pm =
    Just (title, action)
  | otherwise = Nothing

suggestLocalSignature :: Bool -> Maybe HscEnv -> Maybe TcModuleResult -> Maybe Bindings -> Diagnostic -> Maybe (T.Text, TextEdit)
suggestLocalSignature isQuickFix mEnv mTmr mBindings Diagnostic{_message, _range}
  | Just (_ :: T.Text, _ :: T.Text, _ :: T.Text, [identifier]) <-
      (T.unwords . T.words $ _message)
        =~~ ("Polymorphic local binding with no type signature: (.*) ::" :: T.Text)=
    suggestLocalSignature' isQuickFix mEnv mTmr mBindings identifier _range
  | otherwise = Nothing

suggestLocalSignature' :: Bool -> Maybe HscEnv -> Maybe TcModuleResult -> Maybe Bindings -> T.Text -> Range -> Maybe (T.Text, TextEdit)
suggestLocalSignature' isQuickFix mEnv mTmr mBindings identifier Range {_start, _end}
  | Just bindings <- mBindings
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
    Just (title, action)
  | otherwise = Nothing

sameThing :: SrcSpan -> Range -> Bool
sameThing s1 s2 = (_start <$> srcSpanToRange s1) == (_start <$> Just s2)

gblBindingTypeSigToEdit :: GlobalBindingTypeSig -> Maybe PositionMapping -> Maybe TextEdit
gblBindingTypeSigToEdit GlobalBindingTypeSig{..} mmp
  | Just Range{..} <- srcSpanToRange $ getSrcSpan gbName
    , startOfLine <- Position (_line _start) 0
    , beforeLine <- Range startOfLine startOfLine
    -- If `mmp` is `Nothing`, return the original range, it used by lenses from diagnostic,
    -- otherwise we apply `toCurrentRange`, and the guard should fail if `toCurrentRange` failed.
    , Just range <- maybe (Just beforeLine) (flip toCurrentRange beforeLine) mmp
    -- We need to flatten the signature, as otherwise long signatures are
    -- rendered on multiple lines with invalid formatting.
    , renderedFlat <- intercalate " " $ lines gbRendered
    = Just $ TextEdit range $ T.pack renderedFlat <> "\n"
  | otherwise = Nothing

-- |What we need to resolve our lenses, the type of binding it is, and if it's
-- a local binding, it's identifier and range.
data TypeLensesResolveData = TypeLensesResolveLocal {identifier :: T.Text, range :: Range}
                           | TypeLensesResolveGlobal
  deriving (Generic, A.FromJSON, A.ToJSON)

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

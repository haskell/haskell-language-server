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
import           Data.List                            (find)
import qualified Data.Map                             as Map
import           Data.Maybe                           (catMaybes, maybeToList)
import qualified Data.Text                            as T
import           Development.IDE                      (FileDiagnostic (..),
                                                       GhcSession (..),
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
import           Development.IDE.Core.RuleTypes       (TypeCheck (TypeCheck))
import           Development.IDE.Core.Service         (getDiagnostics)
import           Development.IDE.Core.Shake           (getHiddenDiagnostics,
                                                       use)
import qualified Development.IDE.Core.Shake           as Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Util             (printName)
import           Development.IDE.Graph.Classes
import           Development.IDE.Types.Location       (Position (Position, _line),
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
                                                       mkResolveHandler,
                                                       pluginSendRequest)
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
import           Text.Regex.TDFA                      ((=~))

data Log = LogShake Shake.Log deriving Show

instance Pretty Log where
  pretty = \case
    LogShake msg -> pretty msg


typeLensCommandId :: T.Text
typeLensCommandId = "typesignature.add"

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultPluginDescriptor plId desc)
    { pluginHandlers = mkPluginHandler SMethod_TextDocumentCodeLens codeLensProvider
                    <> mkResolveHandler SMethod_CodeLensResolve codeLensResolveProvider
    , pluginCommands = [PluginCommand (CommandId typeLensCommandId) "adds a signature" commandHandler]
    , pluginRules = rules recorder
    , pluginConfigDescriptor = defaultConfigDescriptor {configCustomConfig = mkCustomConfig properties}
    }
  where
    desc = "Provides code lenses type signatures"

properties :: Properties '[ 'PropertyKey "mode" (TEnum Mode)]
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
    -- We have two ways we can possibly generate code lenses for type lenses.
    -- Different options are with different "modes" of the type-lenses plugin.
    -- (Remember here, as the code lens is not resolved yet, we only really need
    -- the range and any data that will help us resolve it later)
    let -- The first option is to generate lens from diagnostics about
        -- top level bindings.
        generateLensFromGlobalDiags diags =
          -- We don't actually pass any data to resolve, however we need this
          -- dummy type to make sure HLS resolves our lens
          [ CodeLens _range Nothing (Just $ toJSON TypeLensesResolve)
            | diag <- diags
            , let lspDiag@Diagnostic {_range} = fdLspDiagnostic diag
            , fdFilePath diag == nfp
            , isGlobalDiagnostic lspDiag]
        -- The second option is to generate lenses from the GlobalBindingTypeSig
        -- rule. This is the only type that needs to have the range adjusted
        -- with PositionMapping.
        -- PositionMapping for diagnostics doesn't make sense, because we always
        -- have fresh diagnostics even if current module parsed failed (the
        -- diagnostic would then be parse failed). See
        -- https://github.com/haskell/haskell-language-server/pull/3558 for this
        -- discussion.
        generateLensFromGlobal sigs mp = do
          [ CodeLens newRange Nothing (Just $ toJSON TypeLensesResolve)
            | sig <- sigs
            , Just range <- [srcSpanToRange (gbSrcSpan sig)]
            , Just newRange <- [toCurrentRange mp range]]
    if mode == Always || mode == Exported
      then do
        -- In this mode we get the global bindings from the
        -- GlobalBindingTypeSigs rule.
        (GlobalBindingTypeSigsResult gblSigs, gblSigsMp) <-
          runActionE "codeLens.GetGlobalBindingTypeSigs" ideState
          $ useWithStaleE GetGlobalBindingTypeSigs nfp
        -- Depending on whether we only want exported or not we filter our list
        -- of signatures to get what we want
        let relevantGlobalSigs =
              if mode == Exported
                then filter gbExported gblSigs
                else gblSigs
        pure $ InL $ generateLensFromGlobal relevantGlobalSigs gblSigsMp
      else do
        -- For this mode we exclusively use diagnostics to create the lenses.
        -- However we will still use the GlobalBindingTypeSigs to resolve them.
        diags <- liftIO $ atomically $ getDiagnostics ideState
        hDiags <- liftIO $ atomically $ getHiddenDiagnostics ideState
        let allDiags = diags <> hDiags
        pure $ InL $ generateLensFromGlobalDiags allDiags

codeLensResolveProvider :: ResolveFunction IdeState TypeLensesResolve Method_CodeLensResolve
codeLensResolveProvider ideState pId lens@CodeLens{_range} uri TypeLensesResolve = do
  nfp <- getNormalizedFilePathE uri
  (gblSigs@(GlobalBindingTypeSigsResult _), pm) <-
    runActionE "codeLens.GetGlobalBindingTypeSigs" ideState
    $ useWithStaleE GetGlobalBindingTypeSigs nfp
  -- regardless of how the original lens was generated, we want to get the range
  -- that the global bindings rule would expect here, hence the need to reverse
  -- position map the range, regardless of whether it was position mapped in the
  -- beginning or freshly taken from diagnostics.
  newRange <- handleMaybe PluginStaleResolve (fromCurrentRange pm _range)
  -- We also pass on the PositionMapping so that the generated text edit can
  -- have the range adjusted.
  (title, edit) <-
        handleMaybe PluginStaleResolve $ suggestGlobalSignature' False (Just gblSigs) (Just pm) newRange
  pure $ lens & L.command ?~ generateLensCommand pId uri title edit

generateLensCommand :: PluginId -> Uri -> T.Text -> TextEdit -> Command
generateLensCommand pId uri title edit =
  let wEdit = WorkspaceEdit (Just $ Map.singleton uri [edit]) Nothing Nothing
  in mkLspCommand pId (CommandId typeLensCommandId) title (Just [toJSON wEdit])

-- Since the lenses are created with diagnostics, and since the globalTypeSig
-- rule can't be changed as it is also used by the hls-refactor plugin, we can't
-- rely on actions. Because we can't rely on actions it doesn't make sense to
-- recompute the edit upon command. Hence the command here just takes a edit
-- and applies it.
commandHandler :: CommandFunction IdeState WorkspaceEdit
commandHandler _ideState _ wedit = do
  _ <- lift $ pluginSendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wedit) (\_ -> pure ())
  pure $ InR Null

--------------------------------------------------------------------------------
suggestSignature :: Bool -> Maybe GlobalBindingTypeSigsResult -> Diagnostic -> [(T.Text, TextEdit)]
suggestSignature isQuickFix mGblSigs diag =
  maybeToList (suggestGlobalSignature isQuickFix mGblSigs diag)

-- The suggestGlobalSignature is separated into two functions. The main function
-- works with a diagnostic, which then calls the secondary function with
-- whatever pieces of the diagnostic it needs. This allows the resolve function,
-- which no longer has the Diagnostic, to still call the secondary functions.
suggestGlobalSignature :: Bool -> Maybe GlobalBindingTypeSigsResult -> Diagnostic -> Maybe (T.Text, TextEdit)
suggestGlobalSignature isQuickFix mGblSigs diag@Diagnostic{_range}
  | isGlobalDiagnostic diag =
    suggestGlobalSignature' isQuickFix mGblSigs Nothing _range
  | otherwise = Nothing

isGlobalDiagnostic :: Diagnostic -> Bool
isGlobalDiagnostic Diagnostic{_message} = _message =~ ("(Top-level binding|Pattern synonym) with no type signature" :: T.Text)

-- If a PositionMapping is supplied, this function will call
-- gblBindingTypeSigToEdit with it to create a TextEdit in the right location.
suggestGlobalSignature' :: Bool -> Maybe GlobalBindingTypeSigsResult -> Maybe PositionMapping -> Range -> Maybe (T.Text, TextEdit)
suggestGlobalSignature' isQuickFix mGblSigs pm range
  |   Just (GlobalBindingTypeSigsResult sigs) <- mGblSigs
    , Just sig <- find (\x -> sameThing (gbSrcSpan x) range) sigs
    , signature <- T.pack $ gbRendered sig
    , title <- if isQuickFix then "add signature: " <> signature else signature
    , Just action <- gblBindingTypeSigToEdit sig pm =
    Just (title, action)
  | otherwise = Nothing

sameThing :: SrcSpan -> Range -> Bool
sameThing s1 s2 = (_start <$> srcSpanToRange s1) == (_start <$> Just s2)

gblBindingTypeSigToEdit :: GlobalBindingTypeSig -> Maybe PositionMapping -> Maybe TextEdit
gblBindingTypeSigToEdit GlobalBindingTypeSig{..} mmp
  | Just Range{..} <- srcSpanToRange $ getSrcSpan gbName
    , startOfLine <- Position (_line _start) 0
    , beforeLine <- Range startOfLine startOfLine
    -- If `mmp` is `Nothing`, return the original range,
    -- otherwise we apply `toCurrentRange`, and the guard should fail if `toCurrentRange` failed.
    , Just range <- maybe (Just beforeLine) (flip toCurrentRange beforeLine) mmp
    -- We need to flatten the signature, as otherwise long signatures are
    -- rendered on multiple lines with invalid formatting.
    , renderedFlat <- unwords $ lines gbRendered
    = Just $ TextEdit range $ T.pack renderedFlat <> "\n"
  | otherwise = Nothing

-- |We don't need anything to resolve our lens, but a data field is mandatory
-- to get types resolved in HLS
data TypeLensesResolve = TypeLensesResolve
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
      bindToSig identifier = liftZonkM $ do
        let name = idName identifier
        hasSig name $ do
          env <- tcInitTidyEnv
          let (_, ty) = tidyOpenType env (idType identifier)
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

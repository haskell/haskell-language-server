{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}
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
import           Control.Lens                        ((^.))
import           Control.Monad                       (forM, mzero)
import           Control.Monad.Extra                 (whenMaybe)
import           Control.Monad.IO.Class              (MonadIO (liftIO))
import           Data.Aeson.Types                    (Value (..), toJSON)
import qualified Data.Aeson.Types                    as A
import           Data.Generics                       (GenericQ, everything, mkQ,
                                                      something)
import qualified Data.HashMap.Strict                 as Map
import           Data.List                           (find)
import           Data.Maybe                          (catMaybes, fromMaybe,
                                                      mapMaybe, maybeToList)
import           Data.String                         (IsString)
import qualified Data.Text                           as T
import           Development.IDE                     (GhcSession (..),
                                                      HscEnvEq (hscEnv),
                                                      RuleResult, Rules, define,
                                                      srcSpanToRange)
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
import           Ide.PluginUtils                     (getNormalizedFilePath,
                                                      handleMaybeM,
                                                      mkLspCommand,
                                                      pluginResponse,
                                                      usePropertyLsp)
import           Ide.Types                           (CommandFunction,
                                                      PluginCommand (PluginCommand),
                                                      PluginDescriptor (..),
                                                      PluginId,
                                                      PluginMethodHandler,
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
                                                      List (..),
                                                      Method (TextDocumentCodeLens),
                                                      ResponseError,
                                                      SMethod (..),
                                                      TextDocumentIdentifier (TextDocumentIdentifier),
                                                      TextEdit (TextEdit),
                                                      WorkspaceEdit (WorkspaceEdit))
import qualified Language.LSP.Types.Lens             as L
import           Text.Regex.TDFA                     ((=~), (=~~))

data Log = LogShake Shake.Log deriving Show
instance Pretty Log where
  pretty = \case
    LogShake log -> pretty log

typeLensCommandId :: IsString s => s
typeLensCommandId = "typesignature.add"

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentCodeLens codeLensProvider
        <> mkPluginHandler STextDocumentCodeLens whereClauseCodeLens
    , pluginCommands = [PluginCommand typeLensCommandId "adds a signature" commandHandler]
    , pluginRules = rules recorder
    , pluginConfigDescriptor = defaultConfigDescriptor {configCustomConfig = mkCustomConfig properties}
    }

properties :: Properties
  '[ 'PropertyKey "whereLensOn" 'TBoolean,
     'PropertyKey "mode" ('TEnum Mode)]
properties = emptyProperties
  & defineEnumProperty #mode "Control how type lenses are shown"
    [ (Always, "Always displays type lenses of global bindings")
    , (Exported, "Only display type lenses of exported global bindings")
    , (Diagnostics, "Follows error messages produced by GHC about missing signatures")
    ] Always
  & defineBooleanProperty #whereLensOn
    "Display type lenses of where bindings"
    True

codeLensProvider ::
  IdeState ->
  PluginId ->
  CodeLensParams ->
  LSP.LspM Config (Either ResponseError (List CodeLens))
codeLensProvider ideState pId CodeLensParams{_textDocument = TextDocumentIdentifier uri} = do
  mode <- usePropertyLsp #mode pId properties
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
          pure (mapMaybe generateLensForGlobal gblSigs')
            <> generateLensFromDiags (suggestLocalSignature False env tmr bindings) -- we still need diagnostics for local bindings
        Exported -> pure $ mapMaybe generateLensForGlobal (filter gbExported gblSigs')
        Diagnostics -> generateLensFromDiags $ suggestSignature False env gblSigs tmr bindings
    Nothing -> pure []

generateLens :: PluginId -> Range -> T.Text -> WorkspaceEdit -> CodeLens
generateLens pId _range title edit =
  let cId = mkLspCommand pId typeLensCommandId title (Just [toJSON edit])
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
  | Just (_ :: T.Text, _ :: T.Text, _ :: T.Text, [identifier] :: [T.Text]) <-
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

ghostSpan :: RealSrcSpan
ghostSpan = realSrcLocSpan $ mkRealSrcLoc "<dummy>" 1 1

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

-- | Get the type string of a binding id
bindToSig :: Id -> HscEnv -> GlobalRdrEnv -> IOEnv (Env TcGblEnv TcLclEnv) String
bindToSig id hsc rdrEnv = do
    env <- tcInitTidyEnv
    let name = idName id
        (_, ty) = tidyOpenType env (idType id)
    pure $ printName name <> " :: " <> showDocRdrEnv hsc rdrEnv (pprSigmaType ty)

gblBindingType :: Maybe HscEnv -> Maybe TcGblEnv -> IO (Maybe GlobalBindingTypeSigsResult)
gblBindingType (Just hsc) (Just gblEnv) = do
  let exports = availsToNameSet $ tcg_exports gblEnv
      sigs = tcg_sigs gblEnv
      binds = collectHsBindsBinders $ tcg_binds gblEnv
      patSyns = tcg_patsyns gblEnv
      rdrEnv = tcg_rdr_env gblEnv
      hasSig :: (Monad m) => Name -> m a -> m (Maybe a)
      hasSig name = whenMaybe (name `elemNameSet` sigs)
      renderBind id = do
        let name = idName id
        hasSig name $ do
          sig <- bindToSig id hsc rdrEnv
          pure $ GlobalBindingTypeSig name sig (name `elemNameSet` exports)
      patToSig p = do
        let name = patSynName p
        hasSig name
            $ pure
            $ GlobalBindingTypeSig
                name
                ("pattern " <> printName name <> " :: " <> showDocRdrEnv hsc rdrEnv (pprPatSynTypeWithoutForalls p))
                (name `elemNameSet` exports)
  (_, maybe [] catMaybes -> bindings) <- initTcWithGbl hsc gblEnv ghostSpan $ mapM renderBind binds
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

-- --------------------------------------------------------------------------------

-- | A binding expression with its id(s) and location.
data WhereBinding = WhereBinding
  { bindingId  :: [Id]
  -- ^ There may multiple ids for one expression.
  -- e.g. @(a,b) = (1,True)@
  , bindingLoc :: SrcSpan
  -- ^ Location for the whole binding.
  -- Here we use the this to render the type signature at the proper place.
  --
  -- Example: For @(a,b) = (1,True)@, it will print the signature after the
  -- open parenthesis instead of the above of the whole expression
  -- if we don't use the binding span.
  }

-- | Existed bindings in a where clause.
data WhereBindings = WhereBindings
  { bindings        :: [WhereBinding]
  , existedSigNames :: [Name]
  -- ^ Names of existing signatures.
  -- It is used to hide type lens for existing signatures.
  --
  -- NOTE: The location of this name is equal to
  -- the binding name.
  --
  -- Example:
  -- @
  -- f :: Int
  -- f = 42
  -- @
  -- The location of signature name `f`(first line) is equal to
  -- the definition of `f`(second line).
  }

-- | All where clauses from type checked source.
findWhereQ :: GenericQ [HsLocalBinds GhcTc]
findWhereQ = everything (<>) $ mkQ [] (pure . findWhere)
  where
    findWhere :: GRHSs GhcTc (LHsExpr GhcTc) -> HsLocalBinds GhcTc
    findWhere = grhssLocalBindsCompat

-- | Find all bindings for **one** where clause.
findBindingsQ :: GenericQ (Maybe WhereBindings)
findBindingsQ = something (mkQ Nothing findBindings)
  where
    findBindings :: NHsValBindsLR GhcTc -> Maybe WhereBindings
    findBindings (NValBinds binds sigs) =
      Just $ WhereBindings
        { bindings = mapMaybe (something (mkQ Nothing findBindingIds) . snd) binds
        , existedSigNames = concatMap findSigIds sigs
        }

    findBindingIds :: LHsBindLR GhcTc GhcTc -> Maybe WhereBinding
    findBindingIds bind = case unLoc bind of
      FunBind{..} -> Just $ WhereBinding (pure $ unLoc fun_id) l
      PatBind{..} ->
        let ids = (everything (<>) $ mkQ [] (maybeToList . findIdFromPat)) pat_lhs
        in Just $ WhereBinding ids l
      _           -> Nothing
      where
        l = getLoc bind

    -- | Example: Find `a` and `b` from @(a,b) = (1,True)@
    findIdFromPat :: Pat GhcTc -> Maybe Id
    findIdFromPat (VarPat _ (L _ id)) = Just id
    findIdFromPat _                   = Nothing

    findSigIds (L _ (TypeSig _ names _)) = map unLoc names
    findSigIds _                         = []

-- | Provide code lens for where bindings.
whereClauseCodeLens :: PluginMethodHandler IdeState TextDocumentCodeLens
whereClauseCodeLens state plId CodeLensParams{..} = do
  enabled <- usePropertyLsp #whereLensOn plId properties
  if not enabled then pure $ pure $ List [] else pluginResponse $ do
    nfp <- getNormalizedFilePath uri
    tmr <- handleMaybeM "Unable to typechecking"
      $ liftIO
      $ runAction "codeLens.local.TypeCheck" state
      $ use TypeCheck nfp
    (hscEnv -> hsc) <- handleMaybeM "Unable to get GhcSession"
      $ liftIO
      $ runAction "codeLens.local.GhcSession" state
      $ use GhcSession nfp
    let tcGblEnv = tmrTypechecked tmr
        rdrEnv = tcg_rdr_env tcGblEnv
        typeCheckedSource = tcg_binds tcGblEnv

        wheres = findWhereQ typeCheckedSource
        localBindings = mapMaybe findBindingsQ wheres

        -- | Note there may multi ids for one binding,
        -- like @(a, b) = (42, True)@, there are `a` and `b`
        -- in one binding.
        bindingToLenses ids span = case srcSpanToRange span of
          Nothing -> pure []
          Just range -> forM ids $ \id -> do
            (_, fromMaybe [] -> sig) <- liftIO
              $ initTcWithGbl hsc tcGblEnv ghostSpan
              $ bindToSig id hsc rdrEnv
            pure $ generateWhereLens plId range (T.pack sig)

    lenses <- concat <$> sequence
      [ bindingToLenses idsWithoutSig bindingLoc
      | WhereBindings{..} <- localBindings
      , let sigSpans = getSrcSpan <$> existedSigNames
      , WhereBinding{..} <- bindings
      , let idsWithoutSig = filter (\x -> getSrcSpan (idName x) `notElem` sigSpans) bindingId
      ]

    pure $ List lenses
    where
      uri = _textDocument ^. L.uri

      generateWhereLens :: PluginId -> Range -> T.Text -> CodeLens
      generateWhereLens plId range title =
        let cmd = mkLspCommand plId typeLensCommandId title (Just [toJSON (makeEdit range title)])
        in  CodeLens range (Just cmd) Nothing

      makeEdit :: Range -> T.Text -> WorkspaceEdit
      makeEdit range text =
        let startPos = range ^. L.start
            insertChar = startPos ^. L.character
            insertRange = Range startPos startPos
        in WorkspaceEdit
          (pure [(uri, List [TextEdit insertRange (text <> "\n" <> T.replicate (fromIntegral insertChar) " ")])])
          Nothing
          Nothing

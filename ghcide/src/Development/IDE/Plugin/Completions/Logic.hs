{-# LANGUAGE CPP        #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE MultiWayIf #-}


-- Mostly taken from "haskell-ide-engine"
module Development.IDE.Plugin.Completions.Logic (
  CachedCompletions
, cacheDataProducer
, localCompletionsForParsedModule
, getCompletions
, fromIdentInfo
) where

import           Control.Applicative
import           Data.Char                                (isUpper)
import           Data.Generics
import           Data.List.Extra                          as List hiding
                                                                  (stripPrefix)
import qualified Data.Map                                 as Map

import           Data.Maybe                               (fromMaybe, isJust,
                                                           mapMaybe)
import qualified Data.Text                                as T
import qualified Text.Fuzzy.Parallel                      as Fuzzy

import           Control.Monad
import           Data.Aeson                               (ToJSON (toJSON))
import           Data.Either                              (fromRight)
import           Data.Function                            (on)
import           Data.Functor
import qualified Data.HashMap.Strict                      as HM
import qualified Data.HashSet                             as HashSet
import           Data.Ord                                 (Down (Down))
import qualified Data.Set                                 as Set
import           Development.IDE.Core.Compile
import           Development.IDE.Core.PositionMapping
import           Development.IDE.GHC.Compat               hiding (ppr)
import qualified Development.IDE.GHC.Compat               as GHC
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.GHC.Error
import           Development.IDE.GHC.Util
import           Development.IDE.Plugin.Completions.Types
import           Development.IDE.Spans.Common
import           Development.IDE.Spans.Documentation
import           Development.IDE.Spans.LocalBindings
import           Development.IDE.Types.Exports
import           Development.IDE.Types.HscEnvEq
import           Development.IDE.Types.Options

#if MIN_VERSION_ghc(9,2,0)
import           GHC.Plugins                              (Depth (AllTheWay),
                                                           defaultSDocContext,
                                                           mkUserStyle,
                                                           neverQualify,
                                                           renderWithContext,
                                                           sdocStyle)
#endif
import           Ide.PluginUtils                          (mkLspCommand)
import           Ide.Types                                (CommandId (..),
                                                           PluginId)
import           Language.LSP.Types
import           Language.LSP.Types.Capabilities
import qualified Language.LSP.VFS                         as VFS
import           Text.Fuzzy.Parallel                      (Scored (score_),
                                                           original)

-- Chunk size used for parallelizing fuzzy matching
chunkSize :: Int
chunkSize = 1000

-- From haskell-ide-engine/hie-plugin-api/Haskell/Ide/Engine/Context.hs

-- | A context of a declaration in the program
-- e.g. is the declaration a type declaration or a value declaration
-- Used for determining which code completions to show
-- TODO: expand this with more contexts like classes or instances for
-- smarter code completion
data Context = TypeContext
             | ValueContext
             | ModuleContext String -- ^ module context with module name
             | ImportContext String -- ^ import context with module name
             | ImportListContext String -- ^ import list context with module name
             | ImportHidingContext String -- ^ import hiding context with module name
             | ExportContext -- ^ List of exported identifiers from the current module
  deriving (Show, Eq)

-- | Generates a map of where the context is a type and where the context is a value
-- i.e. where are the value decls and the type decls
getCContext :: Position -> ParsedModule -> Maybe Context
getCContext pos pm
  | Just (L (locA -> r) modName) <- moduleHeader
  , pos `isInsideSrcSpan` r
  = Just (ModuleContext (moduleNameString modName))

  | Just (L (locA -> r) _) <- exportList
  , pos `isInsideSrcSpan` r
  = Just ExportContext

  | Just ctx <- something (Nothing `mkQ` go `extQ` goInline) decl
  = Just ctx

  | Just ctx <- something (Nothing `mkQ` importGo) imports
  = Just ctx

  | otherwise
  = Nothing

  where decl = hsmodDecls $ unLoc $ pm_parsed_source pm
        moduleHeader = hsmodName $ unLoc $ pm_parsed_source pm
        exportList = hsmodExports $ unLoc $ pm_parsed_source pm
        imports = hsmodImports $ unLoc $ pm_parsed_source pm

        go :: LHsDecl GhcPs -> Maybe Context
        go (L (locA -> r) SigD {})
          | pos `isInsideSrcSpan` r = Just TypeContext
          | otherwise = Nothing
        go (L (locA -> r) GHC.ValD {})
          | pos `isInsideSrcSpan` r = Just ValueContext
          | otherwise = Nothing
        go _ = Nothing

        goInline :: GHC.LHsType GhcPs -> Maybe Context
        goInline (GHC.L (locA -> r) _)
          | pos `isInsideSrcSpan` r = Just TypeContext
        goInline _ = Nothing

        importGo :: GHC.LImportDecl GhcPs -> Maybe Context
        importGo (L (locA -> r) impDecl)
          | pos `isInsideSrcSpan` r
          = importInline importModuleName (fmap (fmap reLoc) $ ideclHiding impDecl)
          <|> Just (ImportContext importModuleName)

          | otherwise = Nothing
          where importModuleName = moduleNameString $ unLoc $ ideclName impDecl

        importInline :: String -> Maybe (Bool,  GHC.Located [LIE GhcPs]) -> Maybe Context
        importInline modName (Just (True, L r _))
          | pos `isInsideSrcSpan` r = Just $ ImportHidingContext modName
          | otherwise = Nothing
        importInline modName (Just (False, L r _))
          | pos `isInsideSrcSpan` r = Just $ ImportListContext modName
          | otherwise = Nothing
        importInline _ _ = Nothing

occNameToComKind :: Maybe T.Text -> OccName -> CompletionItemKind
occNameToComKind ty oc
  | isVarOcc  oc = case occNameString oc of
                     i:_ | isUpper i -> CiConstructor
                     _               -> CiFunction
  | isTcOcc   oc = case ty of
                     Just t
                       | "Constraint" `T.isSuffixOf` t
                       -> CiInterface
                     _ -> CiStruct
  | isDataOcc oc = CiConstructor
  | otherwise    = CiVariable


showModName :: ModuleName -> T.Text
showModName = T.pack . moduleNameString

mkCompl :: PluginId -> IdeOptions -> CompItem -> CompletionItem
mkCompl
  pId
  IdeOptions {..}
  CI
    { compKind,
      isInfix,
      insertText,
      provenance,
      typeText,
      label,
      docs,
      additionalTextEdits
    } = do
  let mbCommand = mkAdditionalEditsCommand pId `fmap` additionalTextEdits
  let ci = CompletionItem
                 {_label = label,
                  _kind = kind,
                  _tags = Nothing,
                  _detail =
                      case (typeText, provenance) of
                          (Just t,_) | not(T.null t) -> Just $ colon <> t
                          (_, ImportedFrom mod)      -> Just $ "from " <> mod
                          (_, DefinedIn mod)         -> Just $ "from " <> mod
                          _                          -> Nothing,
                  _documentation = documentation,
                  _deprecated = Nothing,
                  _preselect = Nothing,
                  _sortText = Nothing,
                  _filterText = Nothing,
                  _insertText = Just insertText,
                  _insertTextFormat = Just Snippet,
                  _insertTextMode = Nothing,
                  _textEdit = Nothing,
                  _additionalTextEdits = Nothing,
                  _commitCharacters = Nothing,
                  _command = mbCommand,
                  _xdata = Nothing}
  removeSnippetsWhen (isJust isInfix) ci

  where kind = Just compKind
        docs' = imported : spanDocToMarkdown docs
        imported = case provenance of
          Local pos  -> "*Defined at " <> pprLineCol (srcSpanStart pos) <> " in this module*\n'"
          ImportedFrom mod -> "*Imported from '" <> mod <> "'*\n"
          DefinedIn mod -> "*Defined in '" <> mod <> "'*\n"
        colon = if optNewColonConvention then ": " else ":: "
        documentation = Just $ CompletionDocMarkup $
                        MarkupContent MkMarkdown $
                        T.intercalate sectionSeparator docs'
        pprLineCol :: SrcLoc -> T.Text
        pprLineCol (UnhelpfulLoc fs) = T.pack $ unpackFS fs
        pprLineCol (RealSrcLoc loc _) =
            "line " <> ppr(srcLocLine loc) <> ", column " <> ppr(srcLocCol loc)


mkAdditionalEditsCommand :: PluginId -> ExtendImport -> Command
mkAdditionalEditsCommand pId edits =
  mkLspCommand pId (CommandId extendImportCommandId) "extend import" (Just [toJSON edits])

mkNameCompItem :: Uri -> Maybe T.Text -> OccName -> Provenance -> Maybe Type -> Maybe Backtick -> SpanDoc -> Maybe (LImportDecl GhcPs) -> CompItem
mkNameCompItem doc thingParent origName provenance thingType isInfix docs !imp = CI {..}
  where
    compKind = occNameToComKind typeText origName
    isTypeCompl = isTcOcc origName
    label = stripPrefix $ showGhc origName
    insertText = case isInfix of
            Nothing -> case getArgText <$> thingType of
                            Nothing      -> label
                            Just argText -> label <> " " <> argText
            Just LeftSide -> label <> "`"

            Just Surrounded -> label
    typeText
          | Just t <- thingType = Just . stripForall $ showGhc t
          | otherwise = Nothing
    additionalTextEdits =
      imp <&> \x ->
        ExtendImport
          { doc,
            thingParent,
            importName = showModName $ unLoc $ ideclName $ unLoc x,
            importQual = getImportQual x,
            newThing = showNameWithoutUniques origName
          }

    stripForall :: T.Text -> T.Text
    stripForall t
      | T.isPrefixOf "forall" t =
        -- We drop 2 to remove the '.' and the space after it
        T.drop 2 (T.dropWhile (/= '.') t)
      | otherwise               = t

    getArgText :: Type -> T.Text
    getArgText typ = argText
      where
        argTypes = getArgs typ
        argText :: T.Text
        argText = mconcat $ List.intersperse " " $ zipWithFrom snippet 1 argTypes
        snippet :: Int -> Type -> T.Text
        snippet i t = case t of
            (TyVarTy _)     -> noParensSnippet
            (LitTy _)       -> noParensSnippet
            (TyConApp _ []) -> noParensSnippet
            _               -> snippetText i ("(" <> showForSnippet t <> ")")
            where
                noParensSnippet = snippetText i (showForSnippet t)
                snippetText i t = "${" <> T.pack (show i) <> ":" <> t <> "}"
        getArgs :: Type -> [Type]
        getArgs t
          | isPredTy t = []
          | isDictTy t = []
          | isForAllTy t = getArgs $ snd (splitForAllTyCoVars t)
          | isFunTy t =
            let (args, ret) = splitFunTys t
              in if isForAllTy ret
                  then getArgs ret
                  else Prelude.filter (not . isDictTy) $ map scaledThing args
          | isPiTy t = getArgs $ snd (splitPiTys t)
#if MIN_VERSION_ghc(8,10,0)
          | Just (Pair _ t) <- coercionKind <$> isCoercionTy_maybe t
          = getArgs t
#else
          | isCoercionTy t = maybe [] (getArgs . snd) (splitCoercionType_maybe t)
#endif
          | otherwise = []


showForSnippet :: Outputable a => a -> T.Text
#if MIN_VERSION_ghc(9,2,0)
showForSnippet x = T.pack $ renderWithContext ctxt $ GHC.ppr x -- FIXme
    where
        ctxt = defaultSDocContext{sdocStyle = mkUserStyle neverQualify AllTheWay}
#else
showForSnippet x = showGhc x
#endif

mkModCompl :: T.Text -> CompletionItem
mkModCompl label =
  CompletionItem label (Just CiModule) Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing

mkModuleFunctionImport :: T.Text -> T.Text -> CompletionItem
mkModuleFunctionImport moduleName label =
  CompletionItem label (Just CiFunction) Nothing (Just moduleName)
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing

mkImportCompl :: T.Text -> T.Text -> CompletionItem
mkImportCompl enteredQual label =
  CompletionItem m (Just CiModule) Nothing (Just label)
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing
  where
    m = fromMaybe "" (T.stripPrefix enteredQual label)

mkExtCompl :: T.Text -> CompletionItem
mkExtCompl label =
  CompletionItem label (Just CiKeyword) Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing


fromIdentInfo :: Uri -> IdentInfo -> Maybe T.Text -> CompItem
fromIdentInfo doc IdentInfo{..} q = CI
  { compKind= occNameToComKind Nothing name
  , insertText=rendered
  , provenance = DefinedIn moduleNameText
  , typeText=Nothing
  , label=rendered
  , isInfix=Nothing
  , docs=emptySpanDoc
  , isTypeCompl= not isDatacon && isUpper (T.head rendered)
  , additionalTextEdits= Just $
        ExtendImport
          { doc,
            thingParent = parent,
            importName = moduleNameText,
            importQual = q,
            newThing = rendered
          }
  }

cacheDataProducer :: Uri -> HscEnvEq -> Module -> GlobalRdrEnv-> GlobalRdrEnv -> [LImportDecl GhcPs] -> IO CachedCompletions
cacheDataProducer uri env curMod globalEnv inScopeEnv limports = do
  let
      packageState = hscEnv env
      curModName = moduleName curMod
      curModNameText = ppr curModName

      importMap = Map.fromList [ (l, imp) | imp@(L (locA -> (RealSrcSpan l _)) _) <- limports ]

      iDeclToModName :: ImportDecl GhcPs -> ModuleName
      iDeclToModName = unLoc . ideclName

      asNamespace :: ImportDecl GhcPs -> ModuleName
      asNamespace imp = maybe (iDeclToModName imp) GHC.unLoc (ideclAs imp)
      -- Full canonical names of imported modules
      importDeclerations = map unLoc limports


      -- The given namespaces for the imported modules (ie. full name, or alias if used)
      allModNamesAsNS = map (showModName . asNamespace) importDeclerations

      rdrElts = globalRdrEnvElts globalEnv

      foldMapM :: (Foldable f, Monad m, Monoid b) => (a -> m b) -> f a -> m b
      foldMapM f xs = foldr step return xs mempty where
        step x r z = f x >>= \y -> r $! z `mappend` y

      getCompls :: [GlobalRdrElt] -> IO ([CompItem],QualCompls)
      getCompls = foldMapM getComplsForOne

      getComplsForOne :: GlobalRdrElt -> IO ([CompItem],QualCompls)
      getComplsForOne (GRE n par True _) =
          (, mempty) <$> toCompItem par curMod curModNameText n Nothing
      getComplsForOne (GRE n par False prov) =
        flip foldMapM (map is_decl prov) $ \spec -> do
          let originalImportDecl = do
                -- we don't want to extend import if it's already in scope
                guard . null $ lookupGRE_Name inScopeEnv n
                -- or if it doesn't have a real location
                loc <- realSpan $ is_dloc spec
                Map.lookup loc importMap
          compItem <- toCompItem par curMod (ppr $ is_mod spec) n originalImportDecl
          let unqual
                | is_qual spec = []
                | otherwise = compItem
              qual
                | is_qual spec = Map.singleton asMod compItem
                | otherwise = Map.fromList [(asMod,compItem),(origMod,compItem)]
              asMod = showModName (is_as spec)
              origMod = showModName (is_mod spec)
          return (unqual,QualCompls qual)

      toCompItem :: Parent -> Module -> T.Text -> Name -> Maybe (LImportDecl GhcPs) -> IO [CompItem]
      toCompItem par m mn n imp' = do
        docs <- getDocumentationTryGhc packageState curMod n
        let (mbParent, originName) = case par of
                            NoParent -> (Nothing, nameOccName n)
                            ParentIs n' -> (Just . T.pack $ printName n', nameOccName n)
#if !MIN_VERSION_ghc(9,2,0)
                            FldParent n' lbl -> (Just . T.pack $ printName n', maybe (nameOccName n) mkVarOccFS lbl)
#endif
        tys <- catchSrcErrors (hsc_dflags packageState) "completion" $ do
                name' <- lookupName packageState m n
                return ( name' >>= safeTyThingType
                       , guard (isJust mbParent) >> name' >>= safeTyThingForRecord
                       )
        let (ty, record_ty) = fromRight (Nothing, Nothing) tys

        let recordCompls = case record_ty of
                Just (ctxStr, flds) | not (null flds) ->
                    [mkRecordSnippetCompItem uri mbParent ctxStr flds (ImportedFrom mn) docs imp']
                _ -> []

        return $ mkNameCompItem uri mbParent originName (ImportedFrom mn) ty Nothing docs imp'
               : recordCompls

  (unquals,quals) <- getCompls rdrElts

  -- The list of all importable Modules from all packages
  moduleNames <- maybe [] (map showModName) <$> envVisibleModuleNames env

  return $ CC
    { allModNamesAsNS = allModNamesAsNS
    , unqualCompls = unquals
    , qualCompls = quals
    , anyQualCompls = []
    , importableModules = moduleNames
    }

-- | Produces completions from the top level declarations of a module.
localCompletionsForParsedModule :: Uri -> ParsedModule -> CachedCompletions
localCompletionsForParsedModule uri pm@ParsedModule{pm_parsed_source = L _ HsModule{hsmodDecls}} =
    CC { allModNamesAsNS = mempty
       , unqualCompls = compls
       , qualCompls = mempty
       , anyQualCompls = []
       , importableModules = mempty
        }
  where
    typeSigIds = Set.fromList
        [ id
            | L _ (SigD _ (TypeSig _ ids _)) <- hsmodDecls
            , L _ id <- ids
            ]
    hasTypeSig = (`Set.member` typeSigIds) . unLoc

    compls = concat
        [ case decl of
            SigD _ (TypeSig _ ids typ) ->
                [mkComp id CiFunction (Just $ showForSnippet typ) | id <- ids]
            ValD _ FunBind{fun_id} ->
                [ mkComp fun_id CiFunction Nothing
                | not (hasTypeSig fun_id)
                ]
            ValD _ PatBind{pat_lhs} ->
                [mkComp id CiVariable Nothing
                | VarPat _ id <- listify (\(_ :: Pat GhcPs) -> True) pat_lhs]
            TyClD _ ClassDecl{tcdLName, tcdSigs} ->
                mkComp tcdLName CiInterface (Just $ showForSnippet tcdLName) :
                [ mkComp id CiFunction (Just $ showForSnippet typ)
                | L _ (ClassOpSig _ _ ids typ) <- tcdSigs
                , id <- ids]
            TyClD _ x ->
                let generalCompls = [mkComp id cl (Just $ showForSnippet $ tyClDeclLName x)
                        | id <- listify (\(_ :: LIdP GhcPs) -> True) x
                        , let cl = occNameToComKind Nothing (rdrNameOcc $ unLoc id)]
                    -- here we only have to look at the outermost type
                    recordCompls = findRecordCompl uri pm (Local pos) x
                in
                   -- the constructors and snippets will be duplicated here giving the user 2 choices.
                   generalCompls ++ recordCompls
            ForD _ ForeignImport{fd_name,fd_sig_ty} ->
                [mkComp fd_name CiVariable (Just $ showForSnippet fd_sig_ty)]
            ForD _ ForeignExport{fd_name,fd_sig_ty} ->
                [mkComp fd_name CiVariable (Just $ showForSnippet fd_sig_ty)]
            _ -> []
            | L (locA -> pos) decl <- hsmodDecls,
            let mkComp = mkLocalComp pos
        ]

    mkLocalComp pos n ctyp ty =
        CI ctyp pn (Local pos) ensureTypeText pn Nothing doc (ctyp `elem` [CiStruct, CiInterface]) Nothing
      where
        -- when sorting completions, we use the presence of typeText
        -- to tell local completions and global completions apart
        -- instead of using the empty string here, we should probably introduce a new field...
        ensureTypeText = Just $ fromMaybe "" ty
        pn = showForSnippet n
        doc = SpanDocText (getDocumentation [pm] $ reLoc n) (SpanDocUris Nothing Nothing)

findRecordCompl :: Uri -> ParsedModule -> Provenance -> TyClDecl GhcPs -> [CompItem]
findRecordCompl uri pmod mn DataDecl {tcdLName, tcdDataDefn} = result
    where
        result = [mkRecordSnippetCompItem uri (Just $ showNameWithoutUniques $ unLoc tcdLName)
                        (showGhc . unLoc $ con_name) field_labels mn doc Nothing
                 | ConDeclH98{..} <- unLoc <$> dd_cons tcdDataDefn
                 , Just  con_details <- [getFlds con_args]
                 , let field_names = concatMap extract con_details
                 , let field_labels = showGhc <$> field_names
                 , (not . List.null) field_labels
                 ]
        doc = SpanDocText (getDocumentation [pmod] $ reLoc tcdLName) (SpanDocUris Nothing Nothing)

        getFlds conArg = case conArg of
                             RecCon rec  -> Just $ unLoc <$> unLoc rec
                             PrefixCon{} -> Just []
                             _           -> Nothing

        extract ConDeclField{..}
            -- NOTE: 'cd_fld_names' is grouped so that the fields
            -- sharing the same type declaration to fit in the same group; e.g.
            --
            -- @
            --   data Foo = Foo {arg1, arg2 :: Int, arg3 :: Int, arg4 :: Bool}
            -- @
            --
            -- is encoded as @[[arg1, arg2], [arg3], [arg4]]@
            -- Hence, we must concat nested arguments into one to get all the fields.
            = map (rdrNameFieldOcc . unLoc) cd_fld_names
        -- XConDeclField
        extract _ = []
findRecordCompl _ _ _ _ = []

ppr :: Outputable a => a -> T.Text
ppr = T.pack . prettyPrint

toggleSnippets :: ClientCapabilities -> CompletionsConfig -> CompletionItem -> CompletionItem
toggleSnippets ClientCapabilities {_textDocument} CompletionsConfig{..} =
  removeSnippetsWhen (not $ enableSnippets && supported)
  where
    supported =
      Just True == (_textDocument >>= _completion >>= _completionItem >>= _snippetSupport)

toggleAutoExtend :: CompletionsConfig -> CompItem -> CompItem
toggleAutoExtend CompletionsConfig{enableAutoExtend=False} x = x {additionalTextEdits = Nothing}
toggleAutoExtend _ x = x

removeSnippetsWhen :: Bool -> CompletionItem -> CompletionItem
removeSnippetsWhen condition x =
  if condition
    then
      x
        { _insertTextFormat = Just PlainText,
          _insertText = Nothing
        }
    else x

-- | Returns the cached completions for the given module and position.
getCompletions
    :: PluginId
    -> IdeOptions
    -> CachedCompletions
    -> Maybe (ParsedModule, PositionMapping)
    -> (Bindings, PositionMapping)
    -> VFS.PosPrefixInfo
    -> ClientCapabilities
    -> CompletionsConfig
    -> HM.HashMap T.Text (HashSet.HashSet IdentInfo)
    -> IO [Scored CompletionItem]
getCompletions plId ideOpts CC {allModNamesAsNS, anyQualCompls, unqualCompls, qualCompls, importableModules}
               maybe_parsed (localBindings, bmapping) prefixInfo caps config moduleExportsMap = do
  let VFS.PosPrefixInfo { fullLine, prefixModule, prefixText } = prefixInfo
      enteredQual = if T.null prefixModule then "" else prefixModule <> "."
      fullPrefix  = enteredQual <> prefixText

      -- Boolean labels to tag suggestions as qualified (or not)
      qual = not(T.null prefixModule)
      notQual = False

      {- correct the position by moving 'foo :: Int -> String ->    '
                                                                    ^
          to                             'foo :: Int -> String ->    '
                                                              ^
      -}
      pos = VFS.cursorPos prefixInfo

      maxC = maxCompletions config

      filtModNameCompls :: [Scored CompletionItem]
      filtModNameCompls =
        (fmap.fmap) mkModCompl
          $ Fuzzy.simpleFilter chunkSize maxC fullPrefix
          $ (if T.null enteredQual then id else mapMaybe (T.stripPrefix enteredQual))
            allModNamesAsNS

      filtCompls = Fuzzy.filter chunkSize maxC prefixText ctxCompls "" "" (label . snd)
        where

          mcc = case maybe_parsed of
            Nothing -> Nothing
            Just (pm, pmapping) ->
              let PositionMapping pDelta = pmapping
                  position' = fromDelta pDelta pos
                  lpos = lowerRange position'
                  hpos = upperRange position'
              in getCContext lpos pm <|> getCContext hpos pm

          -- completions specific to the current context
          ctxCompls' = case mcc of
                        Nothing           -> compls
                        Just TypeContext  -> filter ( isTypeCompl . snd) compls
                        Just ValueContext -> filter (not . isTypeCompl . snd) compls
                        Just _            -> filter (not . isTypeCompl . snd) compls
          -- Add whether the text to insert has backticks
          ctxCompls = (fmap.fmap) (\comp -> toggleAutoExtend config $ comp { isInfix = infixCompls }) ctxCompls'

          infixCompls :: Maybe Backtick
          infixCompls = isUsedAsInfix fullLine prefixModule prefixText pos

          PositionMapping bDelta = bmapping
          oldPos = fromDelta bDelta $ VFS.cursorPos prefixInfo
          startLoc = lowerRange oldPos
          endLoc = upperRange oldPos
          localCompls = map (uncurry localBindsToCompItem) $ getFuzzyScope localBindings startLoc endLoc
          localBindsToCompItem :: Name -> Maybe Type -> CompItem
          localBindsToCompItem name typ = CI ctyp pn thisModName ty pn Nothing emptySpanDoc (not $ isValOcc occ) Nothing
            where
              occ = nameOccName name
              ctyp = occNameToComKind Nothing occ
              pn = showForSnippet name
              ty = showForSnippet <$> typ
              thisModName = Local $ nameSrcSpan name

          compls = if T.null prefixModule
            then map (notQual,) localCompls ++ map (qual,) unqualCompls ++ ((notQual,) . ($Nothing) <$> anyQualCompls)
            else ((qual,) <$> Map.findWithDefault [] prefixModule (getQualCompls qualCompls))
                 ++ ((notQual,) . ($ Just prefixModule) <$> anyQualCompls)

      filtListWith f list =
        [ fmap f label
        | label <- Fuzzy.simpleFilter chunkSize maxC fullPrefix list
        , enteredQual `T.isPrefixOf` original label
        ]

      filtImportCompls = filtListWith (mkImportCompl enteredQual) importableModules
      filterModuleExports moduleName = filtListWith $ mkModuleFunctionImport moduleName
      filtKeywordCompls
          | T.null prefixModule = filtListWith mkExtCompl (optKeywords ideOpts)
          | otherwise = []

  if
    -- TODO: handle multiline imports
    | "import " `T.isPrefixOf` fullLine
      && (List.length (words (T.unpack fullLine)) >= 2)
      && "(" `isInfixOf` T.unpack fullLine
    -> do
      let moduleName = T.pack $ words (T.unpack fullLine) !! 1
          funcs = HM.lookupDefault HashSet.empty moduleName moduleExportsMap
          funs = map (show . name) $ HashSet.toList funcs
      return $ filterModuleExports moduleName $ map T.pack funs
    | "import " `T.isPrefixOf` fullLine
    -> return filtImportCompls
    -- we leave this condition here to avoid duplications and return empty list
    -- since HLS implements these completions (#haskell-language-server/pull/662)
    | "{-# " `T.isPrefixOf` fullLine
    -> return []
    | otherwise -> do
        -- assumes that nubOrdBy is stable
        let uniqueFiltCompls = nubOrdBy (uniqueCompl `on` snd . Fuzzy.original) filtCompls
        let compls = (fmap.fmap.fmap) (mkCompl plId ideOpts) uniqueFiltCompls
        return $
          (fmap.fmap) snd $
          sortBy (compare `on` lexicographicOrdering) $
          mergeListsBy (flip compare `on` score_)
            [ (fmap.fmap) (notQual,) filtModNameCompls
            , (fmap.fmap) (notQual,) filtKeywordCompls
            , (fmap.fmap.fmap) (toggleSnippets caps config) compls
            ]
    where
        -- We use this ordering to alphabetically sort suggestions while respecting
        -- all the previously applied ordering sources. These are:
        --  1. Qualified suggestions go first
        --  2. Fuzzy score ranks next
        --  3. In-scope completions rank next
        --  4. label alphabetical ordering next
        --  4. detail alphabetical ordering (proxy for module)
        lexicographicOrdering Fuzzy.Scored{score_, original} =
          case original of
            (isQual, CompletionItem{_label,_detail}) -> do
              let isLocal = maybe False (":" `T.isPrefixOf`) _detail
              (Down isQual, Down score_, Down isLocal, _label, _detail)



uniqueCompl :: CompItem -> CompItem -> Ordering
uniqueCompl candidate unique =
  case compare (label candidate, compKind candidate)
               (label unique, compKind unique) of
    EQ ->
      -- preserve completions for duplicate record fields where the only difference is in the type
      -- remove redundant completions with less type info than the previous
      if (typeText candidate == typeText unique && isLocalCompletion unique)
        -- filter global completions when we already have a local one
        || not(isLocalCompletion candidate) && isLocalCompletion unique
        then EQ
        else compare (importedFrom candidate, insertText candidate) (importedFrom unique, insertText unique)
    other -> other
  where
      isLocalCompletion ci = isJust(typeText ci)

      importedFrom :: CompItem -> T.Text
      importedFrom (provenance -> ImportedFrom m) = m
      importedFrom (provenance -> DefinedIn m)    = m
      importedFrom (provenance -> Local _)        = "local"
#if __GLASGOW_HASKELL__ < 810
      importedFrom _                              = ""
#endif

-- ---------------------------------------------------------------------
-- helper functions for infix backticks
-- ---------------------------------------------------------------------

hasTrailingBacktick :: T.Text -> Position -> Bool
hasTrailingBacktick line Position { _character=(fromIntegral -> c) }
    | T.length line > c = (line `T.index` c) == '`'
    | otherwise = False

isUsedAsInfix :: T.Text -> T.Text -> T.Text -> Position -> Maybe Backtick
isUsedAsInfix line prefixMod prefixText pos
    | hasClosingBacktick && hasOpeningBacktick = Just Surrounded
    | hasOpeningBacktick = Just LeftSide
    | otherwise = Nothing
  where
    hasOpeningBacktick = openingBacktick line prefixMod prefixText pos
    hasClosingBacktick = hasTrailingBacktick line pos

openingBacktick :: T.Text -> T.Text -> T.Text -> Position -> Bool
openingBacktick line prefixModule prefixText Position { _character=(fromIntegral -> c) }
  | backtickIndex < 0 || backtickIndex >= T.length line = False
  | otherwise = (line `T.index` backtickIndex) == '`'
    where
    backtickIndex :: Int
    backtickIndex =
      let
          prefixLength = T.length prefixText
          moduleLength = if prefixModule == ""
                    then 0
                    else T.length prefixModule + 1 {- Because of "." -}
      in
        -- Points to the first letter of either the module or prefix text
        c - (prefixLength + moduleLength) - 1


-- ---------------------------------------------------------------------

-- | Under certain circumstance GHC generates some extra stuff that we
-- don't want in the autocompleted symbols
    {- When e.g. DuplicateRecordFields is enabled, compiler generates
    names like "$sel:accessor:One" and "$sel:accessor:Two" to disambiguate record selectors
    https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/DuplicateRecordFields#Implementation
    -}
-- TODO: Turn this into an alex lexer that discards prefixes as if they were whitespace.
stripPrefix :: T.Text -> T.Text
stripPrefix name = T.takeWhile (/=':') $ go prefixes
  where
    go [] = name
    go (p:ps)
      | T.isPrefixOf p name = T.drop (T.length p) name
      | otherwise = go ps

-- | Prefixes that can occur in a GHC OccName
prefixes :: [T.Text]
prefixes =
  [
    -- long ones
    "$con2tag_"
  , "$tag2con_"
  , "$maxtag_"

  -- four chars
  , "$sel:"
  , "$tc'"

  -- three chars
  , "$dm"
  , "$co"
  , "$tc"
  , "$cp"
  , "$fx"

  -- two chars
  , "$W"
  , "$w"
  , "$m"
  , "$b"
  , "$c"
  , "$d"
  , "$i"
  , "$s"
  , "$f"
  , "$r"
  , "C:"
  , "N:"
  , "D:"
  , "$p"
  , "$L"
  , "$f"
  , "$t"
  , "$c"
  , "$m"
  ]


safeTyThingForRecord :: TyThing -> Maybe (T.Text, [T.Text])
safeTyThingForRecord (AnId _) = Nothing
safeTyThingForRecord (AConLike dc) =
    let ctxStr =   showGhc . occName . conLikeName $ dc
        field_names = T.pack . unpackFS . flLabel <$> conLikeFieldLabels dc
    in
        Just (ctxStr, field_names)
safeTyThingForRecord _ = Nothing

mkRecordSnippetCompItem :: Uri -> Maybe T.Text -> T.Text -> [T.Text] -> Provenance -> SpanDoc -> Maybe (LImportDecl GhcPs) -> CompItem
mkRecordSnippetCompItem uri parent ctxStr compl importedFrom docs imp = r
  where
      r  = CI {
            compKind = CiSnippet
          , insertText = buildSnippet
          , provenance = importedFrom
          , typeText = Nothing
          , label = ctxStr
          , isInfix = Nothing
          , docs = docs
          , isTypeCompl = False
          , additionalTextEdits = imp <&> \x ->
            ExtendImport
                { doc = uri,
                  thingParent = parent,
                  importName = showModName $ unLoc $ ideclName $ unLoc x,
                  importQual = getImportQual x,
                  newThing = ctxStr
                }
          }

      placeholder_pairs = zip compl ([1..]::[Int])
      snippet_parts = map (\(x, i) -> x <> "=${" <> T.pack (show i) <> ":_" <> x <> "}") placeholder_pairs
      snippet = T.intercalate (T.pack ", ") snippet_parts
      buildSnippet = ctxStr <> " {" <> snippet <> "}"

getImportQual :: LImportDecl GhcPs -> Maybe T.Text
getImportQual (L _ imp)
    | isQualifiedImport imp = Just $ T.pack $ moduleNameString $ maybe (unLoc $ ideclName imp) unLoc (ideclAs imp)
    | otherwise = Nothing

--------------------------------------------------------------------------------

-- This comes from the GHC.Utils.Misc module (not exported)
-- | Merge an unsorted list of sorted lists, for example:
--
--  > mergeListsBy compare [ [2,5,15], [1,10,100] ] = [1,2,5,10,15,100]
--
--  \( O(n \log{} k) \)
mergeListsBy :: forall a. (a -> a -> Ordering) -> [[a]] -> [a]
mergeListsBy cmp all_lists = merge_lists all_lists
  where
    -- Implements "Iterative 2-Way merge" described at
    -- https://en.wikipedia.org/wiki/K-way_merge_algorithm

    -- Merge two sorted lists into one in O(n).
    merge2 :: [a] -> [a] -> [a]
    merge2 [] ys = ys
    merge2 xs [] = xs
    merge2 (x:xs) (y:ys) =
      case cmp x y of
        Prelude.GT -> y : merge2 (x:xs) ys
        _          -> x : merge2 xs (y:ys)

    -- Merge the first list with the second, the third with the fourth, and so
    -- on. The output has half as much lists as the input.
    merge_neighbours :: [[a]] -> [[a]]
    merge_neighbours []   = []
    merge_neighbours [xs] = [xs]
    merge_neighbours (xs : ys : lists) =
      merge2 xs ys : merge_neighbours lists

    -- Since 'merge_neighbours' halves the amount of lists in each iteration,
    -- we perform O(log k) iteration. Each iteration is O(n). The total running
    -- time is therefore O(n log k).
    merge_lists :: [[a]] -> [a]
    merge_lists lists =
      case merge_neighbours lists of
        []     -> []
        [xs]   -> xs
        lists' -> merge_lists lists'

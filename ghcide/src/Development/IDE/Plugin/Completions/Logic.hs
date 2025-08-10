{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiWayIf            #-}

-- Mostly taken from "haskell-ide-engine"
module Development.IDE.Plugin.Completions.Logic (
  CachedCompletions
, cacheDataProducer
, localCompletionsForParsedModule
, getCompletions
, fromIdentInfo
, getCompletionPrefix
, getCompletionPrefixFromRope
) where

import           Control.Applicative
import           Control.Lens                             hiding (Context,
                                                           parts)
import           Data.Char                                (isAlphaNum, isUpper)
import           Data.Default                             (def)
import           Data.Generics
import           Data.List.Extra                          as List hiding
                                                                  (stripPrefix)
import qualified Data.Map                                 as Map
import           Prelude                                  hiding (mod)

import           Data.Maybe                               (fromMaybe, isJust,
                                                           isNothing,
                                                           listToMaybe,
                                                           mapMaybe)
import qualified Data.Text                                as T
import qualified Text.Fuzzy.Parallel                      as Fuzzy

import           Control.Monad
import           Data.Aeson                               (ToJSON (toJSON))
import           Data.Function                            (on)

import qualified Data.HashSet                             as HashSet
import           Data.Ord                                 (Down (Down))
import qualified Data.Set                                 as Set
import           Development.IDE.Core.PositionMapping
import           Development.IDE.GHC.Compat               hiding (isQual, ppr)
import qualified Development.IDE.GHC.Compat               as GHC
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.GHC.Error
import           Development.IDE.GHC.Util
import           Development.IDE.Plugin.Completions.Types
import           Development.IDE.Spans.LocalBindings
import           Development.IDE.Types.Exports
import           Development.IDE.Types.Options
import           GHC.Iface.Ext.Types                      (HieAST,
                                                           NodeInfo (..))
import           GHC.Iface.Ext.Utils                      (nodeInfo)
import           Ide.PluginUtils                          (mkLspCommand)
import           Ide.Types                                (CommandId (..),
                                                           IdePlugins (..),
                                                           PluginId)
import           Language.Haskell.Syntax.Basic
import qualified Language.LSP.Protocol.Lens               as L
import           Language.LSP.Protocol.Types
import qualified Language.LSP.VFS                         as VFS
import           Text.Fuzzy.Parallel                      (Scored (score),
                                                           original)

import qualified Data.Text.Utf16.Rope.Mixed               as Rope
import           Development.IDE                          hiding (line)

import           Development.IDE.Spans.AtPoint            (pointCommand)


import           GHC.Plugins                              (Depth (AllTheWay),
                                                           mkUserStyle,
                                                           neverQualify,
                                                           sdocStyle)

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]


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
          = importInline importModuleName (fmap (fmap reLoc) $ ideclImportList impDecl)
          <|> Just (ImportContext importModuleName)

          | otherwise = Nothing
          where importModuleName = moduleNameString $ unLoc $ ideclName impDecl

        -- importInline :: String -> Maybe (Bool,  GHC.Located [LIE GhcPs]) -> Maybe Context
        importInline modName (Just (EverythingBut, L r _))
          | pos `isInsideSrcSpan` r = Just $ ImportHidingContext modName
          | otherwise = Nothing

        importInline modName (Just (Exactly, L r _))
          | pos `isInsideSrcSpan` r = Just $ ImportListContext modName
          | otherwise = Nothing

        importInline _ _ = Nothing

occNameToComKind :: OccName -> CompletionItemKind
occNameToComKind oc
  | isVarOcc  oc = case occNameString oc of
                     i:_ | isUpper i -> CompletionItemKind_Constructor
                     _               -> CompletionItemKind_Function
  | isTcOcc   oc = CompletionItemKind_Struct
  | isDataOcc oc = CompletionItemKind_Constructor
  | otherwise    = CompletionItemKind_Variable


showModName :: ModuleName -> T.Text
showModName = T.pack . moduleNameString

mkCompl :: Maybe PluginId -- ^ Plugin to use for the extend import command
        -> IdeOptions -> Uri -> CompItem -> CompletionItem
mkCompl
  pId
  _ideOptions
  uri
  CI
    { compKind,
      isInfix,
      insertText,
      provenance,
      label,
      typeText,
      additionalTextEdits,
      nameDetails
    } = do
  let mbCommand = mkAdditionalEditsCommand pId =<< additionalTextEdits
  let ci = CompletionItem
                 {_label = label,
                  _kind = kind,
                  _tags = Nothing,
                  _detail =
                      case (typeText, provenance) of
                          (Just t,_) | not(T.null t) -> Just $ ":: " <> t
                          (_, ImportedFrom mod)      -> Just $ "from " <> mod
                          (_, DefinedIn mod)         -> Just $ "from " <> mod
                          _                          -> Nothing,
                  _documentation = documentation,
                  _deprecated = Nothing,
                  _preselect = Nothing,
                  _sortText = Nothing,
                  _filterText = Nothing,
                  _insertText = Just insertText,
                  _insertTextFormat = Just InsertTextFormat_Snippet,
                  _insertTextMode = Nothing,
                  _textEdit = Nothing,
                  _additionalTextEdits = Nothing,
                  _commitCharacters = Nothing,
                  _command = mbCommand,
                  _data_ = toJSON <$> fmap (CompletionResolveData uri (isNothing typeText)) nameDetails,
                  _labelDetails = Nothing,
                  _textEditText = Nothing}
  removeSnippetsWhen (isJust isInfix) ci

  where kind = Just compKind
        docs' = [imported]
        imported = case provenance of
          Local pos  -> "*Defined at " <> pprLineCol (srcSpanStart pos) <> " in this module*\n"
          ImportedFrom mod -> "*Imported from '" <> mod <> "'*\n"
          DefinedIn mod -> "*Defined in '" <> mod <> "'*\n"
        documentation = Just $ InR $
                        MarkupContent MarkupKind_Markdown $
                        T.intercalate sectionSeparator docs'
        pprLineCol :: SrcLoc -> T.Text
        pprLineCol (UnhelpfulLoc fs) = T.pack $ unpackFS fs
        pprLineCol (RealSrcLoc loc _) =
            "line " <> printOutputable (srcLocLine loc) <> ", column " <> printOutputable (srcLocCol loc)


mkAdditionalEditsCommand :: Maybe PluginId -> ExtendImport -> Maybe Command
mkAdditionalEditsCommand (Just pId) edits = Just $ mkLspCommand pId (CommandId extendImportCommandId) "extend import" (Just [toJSON edits])
mkAdditionalEditsCommand _ _ = Nothing

mkNameCompItem :: Uri -> Maybe T.Text -> OccName -> Provenance -> Maybe Backtick -> Maybe (LImportDecl GhcPs) -> Maybe Module -> CompItem
mkNameCompItem doc thingParent origName provenance isInfix !imp mod = CI {..}
  where
    isLocalCompletion = True
    nameDetails = NameDetails <$> mod <*> pure origName
    compKind = occNameToComKind origName
    isTypeCompl = isTcOcc origName
    typeText = Nothing
    label = stripOccNamePrefix $ printOutputable origName
    insertText = case isInfix of
            Nothing         -> label
            Just LeftSide   -> label <> "`"

            Just Surrounded -> label
    additionalTextEdits =
      imp <&> \x ->
        ExtendImport
          { doc,
            thingParent,
            importName = showModName $ unLoc $ ideclName $ unLoc x,
            importQual = getImportQual x,
            newThing = printOutputable origName
          }

showForSnippet :: Outputable a => a -> T.Text
showForSnippet x = T.pack $ renderWithContext ctxt $ GHC.ppr x -- FIXme
    where
        ctxt = defaultSDocContext{sdocStyle = mkUserStyle neverQualify AllTheWay}

mkModCompl :: T.Text -> CompletionItem
mkModCompl label =
    defaultCompletionItemWithLabel label
    & L.kind ?~ CompletionItemKind_Module

mkModuleFunctionImport :: T.Text -> T.Text -> CompletionItem
mkModuleFunctionImport moduleName label =
    defaultCompletionItemWithLabel label
    & L.kind ?~ CompletionItemKind_Function
    & L.detail ?~ moduleName

mkImportCompl :: T.Text -> T.Text -> CompletionItem
mkImportCompl enteredQual label =
    defaultCompletionItemWithLabel m
    & L.kind ?~ CompletionItemKind_Module
    & L.detail ?~ label
  where
    m = fromMaybe "" (T.stripPrefix enteredQual label)

mkExtCompl :: T.Text -> CompletionItem
mkExtCompl label =
    defaultCompletionItemWithLabel label
    & L.kind ?~ CompletionItemKind_Keyword

defaultCompletionItemWithLabel :: T.Text -> CompletionItem
defaultCompletionItemWithLabel label =
    CompletionItem label def def def def def def def def def
                         def def def def def def def def def

fromIdentInfo :: Uri -> IdentInfo -> Maybe T.Text -> CompItem
fromIdentInfo doc identInfo@IdentInfo{..} q = CI
  { compKind= occNameToComKind name
  , insertText=rend
  , provenance = DefinedIn mod
  , label=rend
  , typeText = Nothing
  , isInfix=Nothing
  , isTypeCompl= not (isDatacon identInfo) && isUpper (T.head rend)
  , additionalTextEdits= Just $
        ExtendImport
          { doc,
            thingParent = occNameText <$> parent,
            importName = mod,
            importQual = q,
            newThing = rend
          }
  , nameDetails = Nothing
  , isLocalCompletion = False
  }
  where rend = rendered identInfo
        mod = moduleNameText identInfo

cacheDataProducer :: Uri -> [ModuleName] -> Module -> GlobalRdrEnv-> GlobalRdrEnv -> [LImportDecl GhcPs] -> CachedCompletions
cacheDataProducer uri visibleMods curMod globalEnv inScopeEnv limports =
  let curModName = moduleName curMod
      curModNameText = printOutputable curModName

      importMap = Map.fromList [ (l, imp) | imp@(L (locA -> (RealSrcSpan l _)) _) <- limports ]

      iDeclToModName :: ImportDecl GhcPs -> ModuleName
      iDeclToModName = unLoc . ideclName

      asNamespace :: ImportDecl GhcPs -> ModuleName
      asNamespace imp = maybe (iDeclToModName imp) GHC.unLoc (ideclAs imp)
      -- Full canonical names of imported modules
      importDeclarations = map unLoc limports


      -- The given namespaces for the imported modules (ie. full name, or alias if used)
      allModNamesAsNS = map (showModName . asNamespace) importDeclarations

      rdrElts = globalRdrEnvElts globalEnv

      -- construct a map from Parents(type) to their fields
      fieldMap = Map.fromListWith (++) $ flip mapMaybe rdrElts $ \elt -> do
        par <- greParent_maybe elt
#if MIN_VERSION_ghc(9,7,0)
        flbl <- greFieldLabel_maybe elt
#else
        flbl <- greFieldLabel elt
#endif
        Just (par,[flLabel flbl])

      getCompls :: [GlobalRdrElt] -> ([CompItem],QualCompls)
      getCompls = foldMap getComplsForOne

      getComplsForOne :: GlobalRdrElt -> ([CompItem],QualCompls)
      getComplsForOne (GRE n par True _) =
          (toCompItem par curMod curModNameText n Nothing, mempty)
      getComplsForOne (GRE n par False prov) =
        flip foldMap (map is_decl prov) $ \spec ->
          let originalImportDecl = do
                -- we don't want to extend import if it's already in scope
                guard . null $ lookupGRE_Name inScopeEnv n
                -- or if it doesn't have a real location
                loc <- realSpan $ is_dloc spec
                Map.lookup loc importMap
              compItem = toCompItem par curMod (printOutputable $ is_mod spec) n originalImportDecl
              unqual
                | is_qual spec = []
                | otherwise = compItem
              qual
                | is_qual spec = Map.singleton asMod compItem
                | otherwise = Map.fromList [(asMod,compItem),(origMod,compItem)]
              asMod = showModName (is_as spec)
#if MIN_VERSION_ghc(9,8,0)
              origMod = showModName (moduleName $ is_mod spec)
#else
              origMod = showModName (is_mod spec)
#endif
          in (unqual,QualCompls qual)

      toCompItem :: Parent -> Module -> T.Text -> Name -> Maybe (LImportDecl GhcPs) -> [CompItem]
      toCompItem par _ mn n imp' =
        -- docs <- getDocumentationTryGhc packageState curMod n
        let (mbParent, originName) = case par of
                            NoParent -> (Nothing, nameOccName n)
                            ParentIs n' -> (Just . T.pack $ printName n', nameOccName n)
            recordCompls = case par of
                ParentIs parent
                  | isDataConName n
                  , Just flds <- Map.lookup parent fieldMap
                  , not (null flds) ->
                    [mkRecordSnippetCompItem uri mbParent (printOutputable originName) (map (T.pack . unpackFS . field_label) flds) (ImportedFrom mn) imp']
                _ -> []

        in mkNameCompItem uri mbParent originName (ImportedFrom mn) Nothing imp' (nameModule_maybe n)
           : recordCompls

      (unquals,quals) = getCompls rdrElts

      -- The list of all importable Modules from all packages
      moduleNames = map showModName visibleMods

  in CC
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
        [ identifier
            | L _ (SigD _ (TypeSig _ ids _)) <- hsmodDecls
            , L _ identifier <- ids
            ]
    hasTypeSig = (`Set.member` typeSigIds) . unLoc

    compls = concat
        [ case decl of
            SigD _ (TypeSig _ ids typ) ->
                [mkComp identifier CompletionItemKind_Function (Just $ showForSnippet typ) | identifier <- ids]
            ValD _ FunBind{fun_id} ->
                [ mkComp fun_id CompletionItemKind_Function Nothing
                | not (hasTypeSig fun_id)
                ]
            ValD _ PatBind{pat_lhs} ->
                [mkComp identifier CompletionItemKind_Variable Nothing
                | VarPat _ identifier <- listify (\(_ :: Pat GhcPs) -> True) pat_lhs]
            TyClD _ ClassDecl{tcdLName, tcdSigs, tcdATs} ->
                mkComp tcdLName CompletionItemKind_Interface (Just $ showForSnippet tcdLName) :
                [ mkComp identifier CompletionItemKind_Function (Just $ showForSnippet typ)
                | L _ (ClassOpSig _ _ ids typ) <- tcdSigs
                , identifier <- ids] ++
                [ mkComp fdLName CompletionItemKind_Struct (Just $ showForSnippet fdLName)
                | L _ (FamilyDecl{fdLName}) <- tcdATs]
            TyClD _ x ->
                let generalCompls = [mkComp identifier cl (Just $ showForSnippet $ tyClDeclLName x)
                        | identifier <- listify (\(_ :: LIdP GhcPs) -> True) x
                        , let cl = occNameToComKind (rdrNameOcc $ unLoc identifier)]
                    -- here we only have to look at the outermost type
                    recordCompls = findRecordCompl uri (Local pos) x
                in
                   -- the constructors and snippets will be duplicated here giving the user 2 choices.
                   generalCompls ++ recordCompls
            ForD _ ForeignImport{fd_name,fd_sig_ty} ->
                [mkComp fd_name CompletionItemKind_Variable (Just $ showForSnippet fd_sig_ty)]
            ForD _ ForeignExport{fd_name,fd_sig_ty} ->
                [mkComp fd_name CompletionItemKind_Variable (Just $ showForSnippet fd_sig_ty)]
            _ -> []
            | L (locA -> pos) decl <- hsmodDecls,
            let mkComp = mkLocalComp pos
        ]

    mkLocalComp pos n ctyp ty =
        CI ctyp pn (Local pos) pn ty Nothing (ctyp `elem` [CompletionItemKind_Struct, CompletionItemKind_Interface]) Nothing (Just $ NameDetails (ms_mod $ pm_mod_summary pm) occ) True
      where
        occ = rdrNameOcc $ unLoc n
        pn = showForSnippet n

findRecordCompl :: Uri -> Provenance -> TyClDecl GhcPs -> [CompItem]
findRecordCompl uri mn DataDecl {tcdLName, tcdDataDefn} = result
    where
        result = [mkRecordSnippetCompItem uri (Just $ printOutputable $ unLoc tcdLName)
                        (printOutputable . unLoc $ con_name) field_labels mn Nothing
                 | ConDeclH98{..} <- unLoc <$> (extract_cons $ dd_cons tcdDataDefn)
                 , Just  con_details <- [getFlds con_args]
                 , let field_names = concatMap extract con_details
                 , let field_labels = printOutputable <$> field_names
                 , (not . List.null) field_labels
                 ]

        getFlds conArg = case conArg of
                             RecCon rec  -> Just $ unLoc <$> unLoc rec
                             PrefixCon{} -> Just []
                             _           -> Nothing

            -- NOTE: 'cd_fld_names' is grouped so that the fields
            -- sharing the same type declaration to fit in the same group; e.g.
            --
            -- @
            --   data Foo = Foo {arg1, arg2 :: Int, arg3 :: Int, arg4 :: Bool}
            -- @
            --
            -- is encoded as @[[arg1, arg2], [arg3], [arg4]]@
            -- Hence, we must concat nested arguments into one to get all the fields.
        extract ConDeclField{..}
            = map (foLabel . unLoc) cd_fld_names
        -- XConDeclField
        extract _ = []
findRecordCompl _ _ _ = []

toggleSnippets :: ClientCapabilities -> CompletionsConfig -> CompletionItem -> CompletionItem
toggleSnippets ClientCapabilities {_textDocument} CompletionsConfig{..} =
  removeSnippetsWhen (not $ enableSnippets && supported)
  where
    supported =
      Just True == (_textDocument >>= _completion >>= view L.completionItem >>= view L.snippetSupport)

toggleAutoExtend :: CompletionsConfig -> CompItem -> CompItem
toggleAutoExtend CompletionsConfig{enableAutoExtend=False} x = x {additionalTextEdits = Nothing}
toggleAutoExtend _ x = x

removeSnippetsWhen :: Bool -> CompletionItem -> CompletionItem
removeSnippetsWhen condition x =
  if condition
    then
      x
        { _insertTextFormat = Just InsertTextFormat_PlainText,
          _insertText = Nothing
        }
    else x

-- | Returns the cached completions for the given module and position.
getCompletions
    :: IdePlugins a
    -> IdeOptions
    -> CachedCompletions
    -> Maybe (ParsedModule, PositionMapping)
    -> Maybe (HieAstResult, PositionMapping)
    -> (Bindings, PositionMapping)
    -> PosPrefixInfo
    -> ClientCapabilities
    -> CompletionsConfig
    -> ModuleNameEnv (HashSet.HashSet IdentInfo)
    -> Uri
    -> [Scored CompletionItem]
getCompletions
    plugins
    ideOpts
    CC {allModNamesAsNS, anyQualCompls, unqualCompls, qualCompls, importableModules}
    maybe_parsed
    maybe_ast_res
    (localBindings, bmapping)
    prefixInfo@(PosPrefixInfo { fullLine, prefixScope, prefixText })
    caps
    config
    moduleExportsMap
    uri
    -- ------------------------------------------------------------------------
    -- IMPORT MODULENAME (NAM|)
    | Just (ImportListContext moduleName) <- maybeContext
    = moduleImportListCompletions moduleName

    | Just (ImportHidingContext moduleName) <- maybeContext
    = moduleImportListCompletions moduleName

    -- ------------------------------------------------------------------------
    -- IMPORT MODULENAM|
    | Just (ImportContext _moduleName) <- maybeContext
    = filtImportCompls

    -- ------------------------------------------------------------------------
    -- {-# LA| #-}
    -- we leave this condition here to avoid duplications and return empty list
    -- since HLS implements these completions (#haskell-language-server/pull/662)
    | "{-# " `T.isPrefixOf` fullLine
    = []

    -- ------------------------------------------------------------------------
    | otherwise =
        -- assumes that nubOrdBy is stable
        let uniqueFiltCompls = nubOrdBy (uniqueCompl `on` snd . Fuzzy.original) filtCompls
            compls = (fmap.fmap.fmap) (mkCompl pId ideOpts uri) uniqueFiltCompls
            pId = lookupCommandProvider plugins (CommandId extendImportCommandId)
        in
          (fmap.fmap) snd $
          sortBy (compare `on` lexicographicOrdering) $
          mergeListsBy (flip compare `on` score)
            [ (fmap.fmap) (notQual,) filtModNameCompls
            , (fmap.fmap) (notQual,) filtKeywordCompls
            , (fmap.fmap.fmap) (toggleSnippets caps config) compls
            ]
    where
      enteredQual = if T.null prefixScope then "" else prefixScope <> "."
      fullPrefix  = enteredQual <> prefixText

      -- Boolean labels to tag suggestions as qualified (or not)
      qual = not(T.null prefixScope)
      notQual = False

      {- correct the position by moving 'foo :: Int -> String ->    '
                                                                    ^
          to                             'foo :: Int -> String ->    '
                                                              ^
      -}
      pos = cursorPos prefixInfo

      maxC = maxCompletions config

      filtModNameCompls :: [Scored CompletionItem]
      filtModNameCompls =
        (fmap.fmap) mkModCompl
          $ Fuzzy.simpleFilter chunkSize maxC fullPrefix
          $ (if T.null enteredQual then id else mapMaybe (T.stripPrefix enteredQual))
            allModNamesAsNS
      -- If we have a parsed module, use it to determine which completion to show.
      maybeContext :: Maybe Context
      maybeContext = case maybe_parsed of
            Nothing -> Nothing
            Just (pm, pmapping) ->
              let PositionMapping pDelta = pmapping
                  position' = fromDelta pDelta pos
                  lpos = lowerRange position'
                  hpos = upperRange position'
              in getCContext lpos pm <|> getCContext hpos pm

      filtCompls :: [Scored (Bool, CompItem)]
      filtCompls = Fuzzy.filter chunkSize maxC prefixText ctxCompls (label . snd)
        where
          -- We need the hieast to be "fresh". We can't get types from "stale" hie files, so hasfield won't work,
          -- since it gets the record fields from the types.
          -- Perhaps this could be fixed with a refactor to GHC's IfaceTyCon, to have it also contain record fields.
          -- Requiring fresh hieast is fine for normal workflows, because it is generated while the user edits.
          recordDotSyntaxCompls :: [(Bool, CompItem)]
          recordDotSyntaxCompls = case maybe_ast_res of
            Just (HAR {hieAst = hieast, hieKind = HieFresh},_) -> concat $ pointCommand hieast (completionPrefixPos prefixInfo) nodeCompletions
            _ -> []
            where
              nodeCompletions :: HieAST Type -> [(Bool, CompItem)]
              nodeCompletions node = concatMap g (nodeType $ nodeInfo node)
              g :: Type -> [(Bool, CompItem)]
              g (TyConApp theTyCon _) = map (dotFieldSelectorToCompl (printOutputable $ GHC.tyConName theTyCon)) $ getSels theTyCon
              g _ = []
              getSels :: GHC.TyCon -> [T.Text]
              getSels tycon = let f fieldLabel = printOutputable fieldLabel
                              in map f $ tyConFieldLabels tycon
              -- Completions can return more information that just the completion itself, but it will
              -- require more than what GHC currently gives us in the HieAST, since it only gives the Type
              -- of the fields, not where they are defined, etc. So for now the extra fields remain empty.
              -- Also: additionalTextEdits is a todo, since we may want to import the record. It requires a way
              -- to get the record's module, which isn't included in the type information used to get the fields.
              dotFieldSelectorToCompl :: T.Text -> T.Text -> (Bool, CompItem)
              dotFieldSelectorToCompl recname label = (True, CI
                { compKind = CompletionItemKind_Field
                , insertText = label
                , provenance = DefinedIn recname
                , label = label
                , typeText = Nothing
                , isInfix = Nothing
                , isTypeCompl = False
                , additionalTextEdits = Nothing
                , nameDetails = Nothing
                , isLocalCompletion = False
                })

          -- completions specific to the current context
          ctxCompls' = case maybeContext of
                        Nothing           -> compls
                        Just TypeContext  -> filter ( isTypeCompl . snd) compls
                        Just ValueContext -> filter (not . isTypeCompl . snd) compls
                        Just _            -> filter (not . isTypeCompl . snd) compls
          -- Add whether the text to insert has backticks
          ctxCompls = (fmap.fmap) (\comp -> toggleAutoExtend config $ comp { isInfix = infixCompls }) ctxCompls'

          infixCompls :: Maybe Backtick
          infixCompls = isUsedAsInfix fullLine prefixScope prefixText pos

          PositionMapping bDelta = bmapping
          oldPos = fromDelta bDelta $ cursorPos prefixInfo
          startLoc = lowerRange oldPos
          endLoc = upperRange oldPos
          localCompls = map (uncurry localBindsToCompItem) $ getFuzzyScope localBindings startLoc endLoc
          localBindsToCompItem :: Name -> Maybe Type -> CompItem
          localBindsToCompItem name typ = CI ctyp pn thisModName pn ty Nothing (not $ isValOcc occ) Nothing dets True
            where
              occ = nameOccName name
              ctyp = occNameToComKind occ
              pn = showForSnippet name
              ty = showForSnippet <$> typ
              thisModName = Local $ nameSrcSpan name
              dets = NameDetails <$> nameModule_maybe name <*> pure (nameOccName name)

          -- When record-dot-syntax completions are available, we return them exclusively.
          -- They are only available when we write i.e. `myrecord.` with OverloadedRecordDot enabled.
          -- Anything that isn't a field is invalid, so those completion don't make sense.
          compls
            | T.null prefixScope = map (notQual,) localCompls ++ map (qual,) unqualCompls ++ map (\compl -> (notQual, compl Nothing)) anyQualCompls
            | not $ null recordDotSyntaxCompls = recordDotSyntaxCompls
            | otherwise = ((qual,) <$> Map.findWithDefault [] prefixScope (getQualCompls qualCompls))
                 ++ map (\compl -> (notQual, compl (Just prefixScope))) anyQualCompls

      filtListWith f xs =
        [ fmap f label
        | label <- Fuzzy.simpleFilter chunkSize maxC fullPrefix xs
        , enteredQual `T.isPrefixOf` original label
        ]

      moduleImportListCompletions :: String -> [Scored CompletionItem]
      moduleImportListCompletions moduleNameS =
        let moduleName = T.pack moduleNameS
            funcs = lookupWithDefaultUFM moduleExportsMap HashSet.empty $ mkModuleName moduleNameS
            funs = map (show . name) $ HashSet.toList funcs
        in filterModuleExports moduleName $ map T.pack funs

      filtImportCompls :: [Scored CompletionItem]
      filtImportCompls = filtListWith (mkImportCompl enteredQual) importableModules

      filterModuleExports :: T.Text -> [T.Text] -> [Scored CompletionItem]
      filterModuleExports moduleName = filtListWith $ mkModuleFunctionImport moduleName

      filtKeywordCompls :: [Scored CompletionItem]
      filtKeywordCompls
          | T.null prefixScope = filtListWith mkExtCompl (optKeywords ideOpts)
          | otherwise = []

      -- We use this ordering to alphabetically sort suggestions while respecting
      -- all the previously applied ordering sources. These are:
      --  1. Qualified suggestions go first
      --  2. Fuzzy score ranks next
      --  3. In-scope completions rank next
      --  4. label alphabetical ordering next
      --  4. detail alphabetical ordering (proxy for module)
      lexicographicOrdering Fuzzy.Scored{score, original} =
        case original of
          (isQual, CompletionItem{_label,_detail}) -> do
            let isLocal = maybe False (":" `T.isPrefixOf`) _detail
            (Down isQual, Down score, Down isLocal, _label, _detail)




uniqueCompl :: CompItem -> CompItem -> Ordering
uniqueCompl candidate unique =
  case compare (label candidate, compKind candidate)
               (label unique, compKind unique) of
    EQ ->
      -- preserve completions for duplicate record fields where the only difference is in the type
      -- remove redundant completions with less type info than the previous
      if isLocalCompletion unique
        -- filter global completions when we already have a local one
        || not(isLocalCompletion candidate) && isLocalCompletion unique
        then EQ
        else compare (importedFrom candidate, insertText candidate) (importedFrom unique, insertText unique)
    other -> other
  where
      importedFrom :: CompItem -> T.Text
      importedFrom (provenance -> ImportedFrom m) = m
      importedFrom (provenance -> DefinedIn m)    = m
      importedFrom (provenance -> Local _)        = "local"

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

mkRecordSnippetCompItem :: Uri -> Maybe T.Text -> T.Text -> [T.Text] -> Provenance -> Maybe (LImportDecl GhcPs) -> CompItem
mkRecordSnippetCompItem uri parent ctxStr compl importedFrom imp = r
  where
      r  = CI {
            compKind = CompletionItemKind_Snippet
          , insertText = buildSnippet
          , provenance = importedFrom
          , typeText = Nothing
          , label = ctxStr
          , isInfix = Nothing
          , isTypeCompl = False
          , additionalTextEdits = imp <&> \x ->
            ExtendImport
                { doc = uri,
                  thingParent = parent,
                  importName = showModName $ unLoc $ ideclName $ unLoc x,
                  importQual = getImportQual x,
                  newThing = ctxStr
                }
          , nameDetails = Nothing
          , isLocalCompletion = True
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

-- |From the given cursor position, gets the prefix module or record for autocompletion
getCompletionPrefix :: Position -> VFS.VirtualFile -> PosPrefixInfo
getCompletionPrefix pos (VFS.VirtualFile _ _ ropetext) = getCompletionPrefixFromRope pos ropetext

getCompletionPrefixFromRope :: Position -> Rope.Rope -> PosPrefixInfo
getCompletionPrefixFromRope pos@(Position l c) ropetext =
      fromMaybe (PosPrefixInfo "" "" "" pos) $ do -- Maybe monad
        let headMaybe = listToMaybe
            lastMaybe = headMaybe . reverse

        -- grab the entire line the cursor is at
        curLine <- headMaybe $ Rope.lines
                             $ fst $ Rope.splitAtLine 1 $ snd $ Rope.splitAtLine (fromIntegral l) ropetext
        let beforePos = T.take (fromIntegral c) curLine
        -- the word getting typed, after previous space and before cursor
        curWord <-
            if | T.null beforePos        -> Just ""
               | T.last beforePos == ' ' -> Just "" -- don't count abc as the curword in 'abc '
               | otherwise               -> lastMaybe (T.words beforePos)

        let parts = T.split (=='.')
                      $ T.takeWhileEnd (\x -> isAlphaNum x || x `elem` ("._'"::String)) curWord
        case reverse parts of
          [] -> Nothing
          (x:xs) -> do
            let modParts = reverse $ filter (not .T.null) xs
                -- Must check the prefix is a valid module name, else record dot accesses treat
                -- the record name as a qualName for search and generated imports
                modName = if all (isUpper . T.head) modParts then T.intercalate "." modParts else ""
            return $ PosPrefixInfo { fullLine = curLine, prefixScope = modName, prefixText = x, cursorPos = pos }

completionPrefixPos :: PosPrefixInfo -> Position
completionPrefixPos PosPrefixInfo { cursorPos = Position ln co, prefixText = str} = Position ln (co - (fromInteger . toInteger . T.length $ str) - 1)

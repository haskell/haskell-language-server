{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Ide.Plugin.LocalCompletions
  (
    descriptor
  ) where

import Control.DeepSeq ( NFData )
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Binary
import Data.Functor
import Data.Hashable
import qualified Data.Text as T
import Data.Typeable
import Development.IDE as D
import Development.IDE.GHC.Compat (ParsedModule(ParsedModule))
import Development.IDE.Spans.Common
import Development.IDE.Spans.Documentation
import Development.IDE.Core.Rules (useE)
import Development.IDE.Core.Shake (
      getDiagnostics
    , getHiddenDiagnostics
    , getIdeOptionsIO
    )
import GHC.Generics
import GHC.Generics as GG
import Ide.Plugin
import Ide.Types
import Language.Haskell.LSP.Types
import Text.Regex.TDFA.Text()

import Control.Applicative
import Data.Char (isAlphaNum, isUpper)
import Data.Generics as G
import Data.List.Extra as List hiding (stripPrefix)
import qualified Data.Map  as Map

import Data.Maybe (listToMaybe, fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Text.Fuzzy as Fuzzy

import HscTypes
import Name
import RdrName
import Type
import Packages
-- #if MIN_GHC_API_VERSION(8,10,0)
-- import Predicate (isDictTy)
-- import Pair
-- import Coercion
-- #endif

import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Capabilities
-- import qualified Language.Haskell.LSP.VFS as VFS
-- import Development.IDE.Core.Compile
import Development.IDE.Core.PositionMapping
-- import Development.IDE.Plugin.Completions.Types
-- import Development.IDE.Spans.Documentation
import Development.IDE.Spans.LocalBindings
import Development.IDE.GHC.Compat as GHC
import Development.IDE.GHC.Error
import Development.IDE.Types.Options
import Development.IDE.Spans.Common
import Development.IDE.GHC.Util
import Outputable (Outputable)
import qualified Data.Set as Set
import ConLike
import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.VFS as VFS

import GhcPlugins (
    liftIO,
    flLabel,
    unpackFS)
import Control.DeepSeq

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor
descriptor plId = (defaultPluginDescriptor plId)
  {
    pluginCommands = []
  , pluginCodeActionProvider = Nothing
  , pluginCodeLensProvider   = Nothing
  , pluginHoverProvider      = Nothing
  , pluginSymbolsProvider    = Nothing
  , pluginCompletionProvider = Just getCompletionsLSP
  }


------------------------
--- Completion Types
------------------------

data Backtick = Surrounded | LeftSide
  deriving (Eq, Ord, Show)


-- | Intermediate Result of Completions
data CachedCompletions = CC
  {
   unqualCompls :: [CompItem]  -- ^ All Possible completion items
  } deriving Show

instance NFData CachedCompletions where
    rnf = rwhnf

instance Monoid CachedCompletions where
    mempty = CC mempty

instance Semigroup CachedCompletions where
    CC a <> CC a' =
        CC (a<>a')


data CompItem = CI
  { compKind :: CompletionItemKind,
    -- | Snippet for the completion
    insertText :: T.Text,
    -- | From where this item is imported from.
    importedFrom :: Either SrcSpan T.Text,
    -- | Available type information.
    typeText :: Maybe T.Text,
    -- | Label to display to the user.
    label :: T.Text,
    -- | Did the completion happen
    -- in the context of an infix notation.
    isInfix :: Maybe Backtick,
    -- | Available documentation.
    docs :: SpanDoc,
    isTypeCompl :: Bool,
    additionalTextEdits :: Maybe [TextEdit]
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- Generating Local Completions via Rules
-- ---------------------------------------------------------------------

produceLocalCompletions :: Rules ()
produceLocalCompletions = do
    define $ \LocalCompletions file -> do
        pm <- useWithStale GetParsedModule file
        case pm of
            Just (pm, _) -> do
                let cdata = localCompletionsForParsedModule pm
                return ([], Just cdata)
            _ -> return ([], Nothing)

-- | Produce completions info for a file
type instance RuleResult LocalCompletions = CachedCompletions

data LocalCompletions = LocalCompletions
     deriving (Eq, Show, Typeable, GG.Generic)
instance Hashable LocalCompletions
instance NFData   LocalCompletions
instance Binary   LocalCompletions


-- | Generate code actions.
getCompletionsLSP
    :: LSP.LspFuncs cofd
    -> IdeState
    -> CompletionParams
    -> IO (Either ResponseError CompletionResponseResult)
getCompletionsLSP lsp ide
  CompletionParams{_textDocument=TextDocumentIdentifier uri
                  ,_position=position
                  ,_context=completionContext} = do
    contents <- LSP.getVirtualFileFunc lsp $ toNormalizedUri uri
    fmap Right $ case (contents, uriToFilePath' uri) of
      (Just cnts, Just path) -> do
        let npath = toNormalizedFilePath' path
        (ideOpts, compls) <- runIdeAction "Completion" (shakeExtras ide) $ do
            opts <- liftIO $ getIdeOptionsIO $ shakeExtras ide
            compls <- useWithStaleFast LocalCompletions npath
            pm <- useWithStaleFast GetParsedModule npath
            binds <- fromMaybe (mempty, zeroMapping) <$> useWithStaleFast GetBindings npath
            pure (opts, fmap (,pm,binds) compls )
        case compls of
          Just ((cci', _), parsedMod, bindMap) -> do
            pfix <- VFS.getCompletionPrefix position cnts
            case (pfix, completionContext) of
              (Just (VFS.PosPrefixInfo _ "" _ _), Just CompletionContext { _triggerCharacter = Just "."})
                -> return (Completions $ List [])
              (Just pfix', _) -> do
                -- let clientCaps = clientCapabilities $ shakeExtras ide
                -- Completions . List <$> getCompletions ideOpts cci' parsedMod bindMap pfix' clientCaps (WithSnippets True)
                return (Completions $ List [])
              _ -> return (Completions $ List [])
          _ -> return (Completions $ List [])
      _ -> return (Completions $ List [])


completion :: CompletionProvider
completion _lf _ide (CompletionParams _doc _pos _mctxt _mt)
    = pure $ Right $ Completions $ List [r]
    where
        r = CompletionItem label kind tags detail documentation deprecated preselect
                           sortText filterText insertText insertTextFormat
                           textEdit additionalTextEdits commitCharacters
                           command xd
        label = "New Local Completions"
        kind = Nothing
        tags = List []
        detail = Nothing
        documentation = Nothing
        deprecated = Nothing
        preselect = Nothing
        sortText = Nothing
        filterText = Nothing
        insertText = Nothing
        insertTextFormat = Nothing
        textEdit = Nothing
        additionalTextEdits = Nothing
        commitCharacters = Nothing
        command = Nothing
        xd = Nothing

-- ---------------------------------------------------------------------
-- Supporting code
------------------------------------------------------------------------

-- | Produces completions from the top level declarations of a module.
localCompletionsForParsedModule :: ParsedModule -> CachedCompletions
localCompletionsForParsedModule pm@ParsedModule{pm_parsed_source = L _ HsModule{hsmodDecls, hsmodName}} =
    CC { unqualCompls = compls }
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
                [mkComp id CiFunction (Just $ ppr typ) | id <- ids]
            ValD _ FunBind{fun_id} ->
                [ mkComp fun_id CiFunction Nothing
                | not (hasTypeSig fun_id)
                ]
            ValD _ PatBind{pat_lhs} ->
                [mkComp id CiVariable Nothing
                | VarPat _ id <- listify (\(_ :: Pat GhcPs) -> True) pat_lhs]
            TyClD _ ClassDecl{tcdLName, tcdSigs} ->
                mkComp tcdLName CiClass Nothing :
                [ mkComp id CiFunction (Just $ ppr typ)
                | L _ (TypeSig _ ids typ) <- tcdSigs
                , id <- ids]
            TyClD _ x ->
                let generalCompls = [mkComp id cl Nothing
                        | id <- listify (\(_ :: Located(IdP GhcPs)) -> True) x
                        , let cl = occNameToComKind Nothing (rdrNameOcc $ unLoc id)]
                    -- here we only have to look at the outermost type
                    recordCompls = findRecordCompl pm thisModName x
                in
                   -- the constructors and snippets will be duplicated here giving the user 2 choices.
                   generalCompls ++ recordCompls
            ForD _ ForeignImport{fd_name,fd_sig_ty} ->
                [mkComp fd_name CiVariable (Just $ ppr fd_sig_ty)]
            ForD _ ForeignExport{fd_name,fd_sig_ty} ->
                [mkComp fd_name CiVariable (Just $ ppr fd_sig_ty)]
            _ -> []
            | L _ decl <- hsmodDecls
        ]

    mkComp n ctyp ty =
        CI ctyp pn (Right thisModName) ty pn Nothing doc (ctyp `elem` [CiStruct, CiClass]) Nothing
      where
        pn = ppr n
        doc = SpanDocText (getDocumentation [pm] n) (SpanDocUris Nothing Nothing)

    thisModName = ppr hsmodName

findRecordCompl :: ParsedModule -> T.Text -> TyClDecl GhcPs -> [CompItem]
findRecordCompl pmod mn DataDecl {tcdLName, tcdDataDefn} = result
    where
        result = [mkRecordSnippetCompItem (T.pack . showGhc . unLoc $ con_name) field_labels mn doc Nothing
                 | ConDeclH98{..} <- unLoc <$> dd_cons tcdDataDefn
                 , Just  con_details <- [getFlds con_args]
                 , let field_names = mapMaybe extract con_details
                 , let field_labels = T.pack . showGhc . unLoc <$> field_names
                 , (not . List.null) field_labels
                 ]
        doc = SpanDocText (getDocumentation [pmod] tcdLName) (SpanDocUris Nothing Nothing)

        getFlds :: HsConDetails arg (Located [LConDeclField GhcPs]) -> Maybe [ConDeclField GhcPs]
        getFlds conArg = case conArg of
                             RecCon rec -> Just $ unLoc <$> unLoc rec
                             PrefixCon _ -> Just []
                             _ -> Nothing

        extract ConDeclField{..}
             -- TODO: Why is cd_fld_names a list?
            | Just fld_name <- rdrNameFieldOcc . unLoc <$> listToMaybe cd_fld_names = Just fld_name
            | otherwise = Nothing
        -- XConDeclField
        extract _ = Nothing
findRecordCompl _ _ _ = []


ppr :: Outputable a => a -> T.Text
ppr = T.pack . prettyPrint

occNameToComKind :: Maybe T.Text -> OccName -> CompletionItemKind
occNameToComKind ty oc
  | isVarOcc  oc = case occNameString oc of
                     i:_ | isUpper i -> CiConstructor
                     _               -> CiFunction
  | isTcOcc   oc = case ty of
                     Just t
                       | "Constraint" `T.isSuffixOf` t
                       -> CiClass
                     _ -> CiStruct
  | isDataOcc oc = CiConstructor
  | otherwise    = CiVariable


mkRecordSnippetCompItem :: T.Text -> [T.Text] -> T.Text -> SpanDoc -> Maybe (LImportDecl GhcPs) -> CompItem
mkRecordSnippetCompItem ctxStr compl mn docs imp = r
  where
    r =
      CI
        { compKind = CiSnippet,
          insertText = buildSnippet,
          importedFrom = importedFrom,
          typeText = Nothing,
          label = ctxStr,
          isInfix = Nothing,
          docs = docs,
          isTypeCompl = False,
          additionalTextEdits = imp >>= extendImportList (T.unpack ctxStr)
        }

    placeholder_pairs = zip compl ([1 ..] :: [Int])
    snippet_parts = map (\(x, i) -> x <> "=${" <> T.pack (show i) <> ":_" <> x <> "}") placeholder_pairs
    snippet = T.intercalate (T.pack ", ") snippet_parts
    buildSnippet = ctxStr <> " {" <> snippet <> "}"
    importedFrom = Right mn


extendImportList :: String -> LImportDecl GhcPs -> Maybe [TextEdit]
extendImportList name lDecl = let
    f (Just range) ImportDecl {ideclHiding} = case ideclHiding of
        Just (False, x)
          | Set.notMember name (Set.fromList [show y| y <- unLoc x])
          -> let
            start_pos = _end range
            new_start_pos = start_pos {_character = _character start_pos - 1}
            -- use to same start_pos to handle situation where we do not have latest edits due to caching of Rules
            new_range = Range new_start_pos new_start_pos
            -- we cannot wrap mapM_ inside (mapM_) but we need to wrap (<$)
            alpha = all isAlphaNum $ filter (\c -> c /= '_') name
            result = if alpha then name ++ ", "
                else "(" ++ name ++ "), "
            in Just [TextEdit new_range (T.pack result)]
          | otherwise -> Nothing
        _ -> Nothing  -- hiding import list and no list
    f _ _ = Nothing
    src_span = srcSpanToRange . getLoc $ lDecl
    in f src_span . unLoc $ lDecl


--- Completions that are returned and related functions

-- | A context of a declaration in the program
-- e.g. is the declaration a type declaration or a value declaration
-- Used for determining which code completions to show
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
  | Just (L r modName) <- moduleHeader
  , pos `isInsideSrcSpan` r
  = Just (ModuleContext (moduleNameString modName))

  | Just (L r _) <- exportList
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
        go (L r SigD {})
          | pos `isInsideSrcSpan` r = Just TypeContext
          | otherwise = Nothing
        go (L r GHC.ValD {})
          | pos `isInsideSrcSpan` r = Just ValueContext
          | otherwise = Nothing
        go _ = Nothing

        goInline :: GHC.LHsType GhcPs -> Maybe Context
        goInline (GHC.L r _)
          | pos `isInsideSrcSpan` r = Just TypeContext
        goInline _ = Nothing

        importGo :: GHC.LImportDecl GhcPs -> Maybe Context
        importGo (L r impDecl)
          | pos `isInsideSrcSpan` r
          = importInline importModuleName (ideclHiding impDecl)
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


-- | Returns the cached completions for the given module and position.
getCompletions
    :: IdeOptions
    -> CachedCompletions
    -> Maybe (ParsedModule, PositionMapping)
    -> (Bindings, PositionMapping)
    -> VFS.PosPrefixInfo
    -> ClientCapabilities
    -> WithSnippets
    -> IO [CompletionItem]
getCompletions ideOpts CC { unqualCompls }
               maybe_parsed (localBindings, bmapping) prefixInfo caps withSnippets = do
  let VFS.PosPrefixInfo { fullLine, prefixModule, prefixText } = prefixInfo
      enteredQual = if T.null prefixModule then "" else prefixModule <> "."
      fullPrefix  = enteredQual <> prefixText

      {- correct the position by moving 'foo :: Int -> String ->    '
                                                                    ^
          to                             'foo :: Int -> String ->    '
                                                              ^
      -}
      pos = VFS.cursorPos prefixInfo

      filtCompls = map Fuzzy.original $ Fuzzy.filter prefixText ctxCompls "" "" label False
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
                        Nothing -> compls
                        Just TypeContext -> filter isTypeCompl compls
                        Just ValueContext -> filter (not . isTypeCompl) compls
                        Just _ -> filter (not . isTypeCompl) compls
          -- Add whether the text to insert has backticks
          ctxCompls = map (\comp -> comp { isInfix = infixCompls }) ctxCompls'

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
              pn = ppr name
              ty = ppr <$> typ
              thisModName = case nameModule_maybe name of
                Nothing -> Left $ nameSrcSpan name
                Just m -> Right $ ppr m

          compls = if T.null prefixModule
            then localCompls
            else []

      filtListWith f list =
        [ f label
        | label <- Fuzzy.simpleFilter fullPrefix list
        , enteredQual `T.isPrefixOf` label
        ]

      filtListWithSnippet f list suffix =
        [ toggleSnippets caps withSnippets (f label (snippet <> (suffix:: T.Text)))
        | (snippet, label) <- list
        , Fuzzy.test fullPrefix label
        ]

      filtKeywordCompls
          | T.null prefixModule = filtListWith mkExtCompl (optKeywords ideOpts)
          | otherwise = []

      stripLeading :: Char -> String -> String
      stripLeading _ [] = []
      stripLeading c (s:ss)
        | s == c = ss
        | otherwise = s:ss

      result
        | "import " `T.isPrefixOf` fullLine
        = []
        | "{-#" `T.isPrefixOf` T.toLower fullLine
        = []
        | otherwise
        = let uniqueFiltCompls = nubOrdOn insertText filtCompls
          in map (toggleSnippets caps withSnippets
                     . mkCompl ideOpts . stripAutoGenerated) uniqueFiltCompls
             ++ filtKeywordCompls
  return result


mkCompl :: IdeOptions -> CompItem -> CompletionItem
mkCompl IdeOptions{..} CI{compKind,insertText, importedFrom,typeText,label,docs, additionalTextEdits} =
  CompletionItem {_label = label,
                  _kind = kind,
                  _tags = List [],
                  _detail = (colon <>) <$> typeText,
                  _documentation = documentation,
                  _deprecated = Nothing,
                  _preselect = Nothing,
                  _sortText = Nothing,
                  _filterText = Nothing,
                  _insertText = Just insertText,
                  _insertTextFormat = Just Snippet,
                  _textEdit = Nothing,
                  _additionalTextEdits = List <$> additionalTextEdits,
                  _commitCharacters = Nothing,
                  _command = Nothing,
                  _xdata = Nothing}

  where kind = Just compKind
        docs' = imported : spanDocToMarkdown docs
        imported = case importedFrom of
          Left pos -> "*Defined at '" <> ppr pos <> "'*\n'"
          Right mod -> "*Defined in '" <> mod <> "'*\n"
        colon = if optNewColonConvention then ": " else ":: "
        documentation = Just $ CompletionDocMarkup $
                        MarkupContent MkMarkdown $
                        T.intercalate sectionSeparator docs'

-- TODO: We probably don't need to this function in this module
mkExtCompl :: T.Text -> CompletionItem
mkExtCompl label =
  CompletionItem
    label
    (Just CiKeyword)
    (List [])
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing


--- helper functions that will be useful for non-local completions as well

hasTrailingBacktick :: T.Text -> Position -> Bool
hasTrailingBacktick line Position { _character }
    | T.length line > _character = (line `T.index` _character) == '`'
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
openingBacktick line prefixModule prefixText Position { _character }
  | backtickIndex < 0 = False
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
        _character - (prefixLength + moduleLength) - 1

toggleSnippets :: ClientCapabilities -> WithSnippets -> CompletionItem -> CompletionItem
toggleSnippets ClientCapabilities {_textDocument} (WithSnippets with) x
  | with && supported = x
  | otherwise =
    x
      { _insertTextFormat = Just PlainText,
        _insertText = Nothing
      }
  where
    supported =
      Just True == (_textDocument >>= _completion >>= _completionItem >>= _snippetSupport)

-- | Under certain circumstance GHC generates some extra stuff that we
-- don't want in the autocompleted symbols
stripAutoGenerated :: CompItem -> CompItem
stripAutoGenerated ci =
    ci {label = stripPrefix (label ci)}
    {- When e.g. DuplicateRecordFields is enabled, compiler generates
    names like "$sel:accessor:One" and "$sel:accessor:Two" to disambiguate record selectors
    https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/DuplicateRecordFields#Implementation
    -}

stripPrefix :: T.Text -> T.Text
stripPrefix name = T.takeWhile (/= ':') $ go prefixes
  where
    go [] = name
    go (p : ps)
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

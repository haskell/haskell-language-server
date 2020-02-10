{-# LANGUAGE CPP #-}
#include "ghc-api-version.h"
-- Mostly taken from "haskell-ide-engine"
module Development.IDE.Plugin.Completions.Logic (
  CachedCompletions
, cacheDataProducer
, WithSnippets(..)
,getCompletions
) where

import Control.Applicative
import Data.Char (isSpace, isUpper)
import Data.Generics
import Data.List as List hiding (stripPrefix)
import qualified Data.Map  as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Text.Fuzzy as Fuzzy

import GHC
import HscTypes
import Name
import RdrName
import TcRnTypes
import Type
import Var
import Packages
import DynFlags
#if MIN_GHC_API_VERSION(8,10,0)
import Predicate (isDictTy)
import GHC.Platform
import Pair
import Coercion
#endif

import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Capabilities
import qualified Language.Haskell.LSP.VFS as VFS
import Development.IDE.Plugin.Completions.Types
import Development.IDE.Spans.Documentation
import Development.IDE.GHC.Error
import Development.IDE.Types.Options
import Development.IDE.Spans.Common
import Development.IDE.GHC.Util

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

mkCompl :: IdeOptions -> CompItem -> CompletionItem
mkCompl IdeOptions{..} CI{origName,importedFrom,thingType,label,isInfix,docs} =
  CompletionItem label kind ((colon <>) <$> typeText)
    (Just $ CompletionDocMarkup $ MarkupContent MkMarkdown $ T.intercalate sectionSeparator docs')
    Nothing Nothing Nothing Nothing (Just insertText) (Just Snippet)
    Nothing Nothing Nothing Nothing Nothing
  where kind = Just $ occNameToComKind typeText $ occName origName
        insertText = case isInfix of
            Nothing -> case getArgText <$> thingType of
                            Nothing -> label
                            Just argText -> label <> " " <> argText
            Just LeftSide -> label <> "`"

            Just Surrounded -> label
        typeText
          | Just t <- thingType = Just . stripForall $ T.pack (showGhc t)
          | otherwise = Nothing
        docs' = ("*Defined in '" <> importedFrom <> "'*\n") : spanDocToMarkdown docs
        colon = if optNewColonConvention then ": " else ":: "

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
    argText =  mconcat $ List.intersperse " " $ zipWith snippet [1..] argTypes
    snippet :: Int -> Type -> T.Text
    snippet i t = T.pack $ "${" <> show i <> ":" <> showGhc t <> "}"
    getArgs :: Type -> [Type]
    getArgs t
      | isPredTy t = []
      | isDictTy t = []
      | isForAllTy t = getArgs $ snd (splitForAllTys t)
      | isFunTy t =
        let (args, ret) = splitFunTys t
          in if isForAllTy ret
              then getArgs ret
              else Prelude.filter (not . isDictTy) args
      | isPiTy t = getArgs $ snd (splitPiTys t)
#if MIN_GHC_API_VERSION(8,10,0)
      | Just (Pair _ t) <- coercionKind <$> isCoercionTy_maybe t
      = getArgs t
#else
      | isCoercionTy t = maybe [] (getArgs . snd) (splitCoercionType_maybe t)
#endif
      | otherwise = []

mkModCompl :: T.Text -> CompletionItem
mkModCompl label =
  CompletionItem label (Just CiModule) Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing

mkImportCompl :: T.Text -> T.Text -> CompletionItem
mkImportCompl enteredQual label =
  CompletionItem m (Just CiModule) (Just label)
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing
  where 
    m = fromMaybe "" (T.stripPrefix enteredQual label)

mkExtCompl :: T.Text -> CompletionItem
mkExtCompl label =
  CompletionItem label (Just CiKeyword) Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing

mkPragmaCompl :: T.Text -> T.Text -> CompletionItem
mkPragmaCompl label insertText =
  CompletionItem label (Just CiKeyword) Nothing
    Nothing Nothing Nothing Nothing Nothing (Just insertText) (Just Snippet)
    Nothing Nothing Nothing Nothing Nothing

cacheDataProducer :: HscEnv -> DynFlags -> TypecheckedModule -> [TypecheckedModule] -> IO CachedCompletions
cacheDataProducer packageState dflags tm tcs = do
  let parsedMod = tm_parsed_module tm
      curMod = moduleName $ ms_mod $ pm_mod_summary parsedMod
      Just (_,limports,_,_) = tm_renamed_source tm

      iDeclToModName :: ImportDecl name -> ModuleName
      iDeclToModName = unLoc . ideclName

      showModName :: ModuleName -> T.Text
      showModName = T.pack . moduleNameString

      asNamespace :: ImportDecl name -> ModuleName
      asNamespace imp = maybe (iDeclToModName imp) GHC.unLoc (ideclAs imp)
      -- Full canonical names of imported modules
      importDeclerations = map unLoc limports

      -- The list of all importable Modules from all packages
      moduleNames = map showModName (listVisibleModuleNames dflags)

      -- The given namespaces for the imported modules (ie. full name, or alias if used)
      allModNamesAsNS = map (showModName . asNamespace) importDeclerations

      typeEnv = tcg_type_env $ fst $ tm_internals_ tm
      rdrEnv = tcg_rdr_env $ fst $ tm_internals_ tm
      rdrElts = globalRdrEnvElts rdrEnv

      foldMapM :: (Foldable f, Monad m, Monoid b) => (a -> m b) -> f a -> m b
      foldMapM f xs = foldr step return xs mempty where
        step x r z = f x >>= \y -> r $! z `mappend` y

      getCompls :: [GlobalRdrElt] -> IO ([CompItem],QualCompls)
      getCompls = foldMapM getComplsForOne

      getComplsForOne :: GlobalRdrElt -> IO ([CompItem],QualCompls)
      getComplsForOne (GRE n _ True _) =
        case lookupTypeEnv typeEnv n of
          Just tt -> case safeTyThingId tt of
            Just var -> (\x -> ([x],mempty)) <$> varToCompl var
            Nothing -> (\x -> ([x],mempty)) <$> toCompItem curMod n
          Nothing -> (\x -> ([x],mempty)) <$> toCompItem curMod n
      getComplsForOne (GRE n _ False prov) =
        flip foldMapM (map is_decl prov) $ \spec -> do
          compItem <- toCompItem (is_mod spec) n
          let unqual
                | is_qual spec = []
                | otherwise = [compItem]
              qual
                | is_qual spec = Map.singleton asMod [compItem]
                | otherwise = Map.fromList [(asMod,[compItem]),(origMod,[compItem])]
              asMod = showModName (is_as spec)
              origMod = showModName (is_mod spec)
          return (unqual,QualCompls qual)

      varToCompl :: Var -> IO CompItem
      varToCompl var = do
        let typ = Just $ varType var
            name = Var.varName var
            label = T.pack $ showGhc name
        docs <- runGhcEnv packageState $ getDocumentationTryGhc (tm:tcs) name
        return $ CI name (showModName curMod) typ label Nothing docs

      toCompItem :: ModuleName -> Name -> IO CompItem
      toCompItem mn n = do
        docs <- runGhcEnv packageState $ getDocumentationTryGhc (tm:tcs) n
-- lookupName uses runInteractiveHsc, i.e., GHCi stuff which does not work with GHCi
-- and leads to fun errors like "Cannot continue after interface file error".
#ifdef GHC_LIB
        let ty = Right Nothing
#else
        ty <- runGhcEnv packageState $ catchSrcErrors "completion" $ do
                name' <- lookupName n
                return $ name' >>= safeTyThingType
#endif
        return $ CI n (showModName mn) (either (const Nothing) id ty) (T.pack $ showGhc n) Nothing docs

  (unquals,quals) <- getCompls rdrElts

  return $ CC
    { allModNamesAsNS = allModNamesAsNS
    , unqualCompls = unquals
    , qualCompls = quals
    , importableModules = moduleNames
    }

newtype WithSnippets = WithSnippets Bool

toggleSnippets :: ClientCapabilities -> WithSnippets -> CompletionItem -> CompletionItem
toggleSnippets ClientCapabilities { _textDocument } (WithSnippets with) x
  | with && supported = x
  | otherwise = x { _insertTextFormat = Just PlainText
                  , _insertText       = Nothing
                  }
  where supported = fromMaybe False (_textDocument >>= _completion >>= _completionItem >>= _snippetSupport)

-- | Returns the cached completions for the given module and position.
getCompletions :: IdeOptions -> CachedCompletions -> TypecheckedModule -> VFS.PosPrefixInfo -> ClientCapabilities -> WithSnippets -> IO [CompletionItem]
getCompletions ideOpts CC { allModNamesAsNS, unqualCompls, qualCompls, importableModules }
               tm prefixInfo caps withSnippets = do
  let VFS.PosPrefixInfo { VFS.fullLine, VFS.prefixModule, VFS.prefixText } = prefixInfo
      enteredQual = if T.null prefixModule then "" else prefixModule <> "."
      fullPrefix  = enteredQual <> prefixText

      -- default to value context if no explicit context
      context = fromMaybe ValueContext $ getCContext pos (tm_parsed_module tm)

      {- correct the position by moving 'foo :: Int -> String ->    '
                                                                    ^
          to                             'foo :: Int -> String ->    '
                                                              ^
      -}
      pos =
        let Position l c = VFS.cursorPos prefixInfo
            typeStuff = [isSpace, (`elem` (">-." :: String))]
            stripTypeStuff = T.dropWhileEnd (\x -> any (\f -> f x) typeStuff)
            -- if oldPos points to
            -- foo -> bar -> baz
            --    ^
            -- Then only take the line up to there, discard '-> bar -> baz'
            partialLine = T.take c fullLine
            -- drop characters used when writing incomplete type sigs
            -- like '-> '
            d = T.length fullLine - T.length (stripTypeStuff partialLine)
        in Position l (c - d)

      filtModNameCompls =
        map mkModCompl
          $ mapMaybe (T.stripPrefix enteredQual)
          $ Fuzzy.simpleFilter fullPrefix allModNamesAsNS

      filtCompls = map Fuzzy.original $ Fuzzy.filter prefixText ctxCompls "" "" label False
        where
          isTypeCompl = isTcOcc . occName . origName
          -- completions specific to the current context
          ctxCompls' = case context of
                        TypeContext -> filter isTypeCompl compls
                        ValueContext -> filter (not . isTypeCompl) compls
                        _ -> filter (not . isTypeCompl) compls
          -- Add whether the text to insert has backticks
          ctxCompls = map (\comp -> comp { isInfix = infixCompls }) ctxCompls'

          infixCompls :: Maybe Backtick
          infixCompls = isUsedAsInfix fullLine prefixModule prefixText (VFS.cursorPos prefixInfo)

          compls = if T.null prefixModule
            then unqualCompls
            else Map.findWithDefault [] prefixModule $ getQualCompls qualCompls

      filtListWith f list =
        [ f label
        | label <- Fuzzy.simpleFilter fullPrefix list
        , enteredQual `T.isPrefixOf` label
        ]

      filtListWithSnippet f list suffix =
        [ toggleSnippets caps withSnippets (f label (snippet <> suffix))
        | (snippet, label) <- list
        , Fuzzy.test fullPrefix label
        ]

      filtImportCompls = filtListWith (mkImportCompl enteredQual) importableModules
      filtPragmaCompls = filtListWithSnippet mkPragmaCompl validPragmas
      filtOptsCompls   = filtListWith mkExtCompl
      filtKeywordCompls = if T.null prefixModule then filtListWith mkExtCompl keywords else []

      stripLeading :: Char -> String -> String
      stripLeading _ [] = []
      stripLeading c (s:ss)
        | s == c = ss
        | otherwise = s:ss

      result
        | "import " `T.isPrefixOf` fullLine
        = filtImportCompls
        | "{-# language" `T.isPrefixOf` T.toLower fullLine
        = filtOptsCompls languagesAndExts
        | "{-# options_ghc" `T.isPrefixOf` T.toLower fullLine
        = filtOptsCompls (map (T.pack . stripLeading '-') $ flagsForCompletion False)
        | "{-# " `T.isPrefixOf` fullLine
        = filtPragmaCompls (pragmaSuffix fullLine)
        | otherwise
        = filtModNameCompls ++ map (toggleSnippets caps withSnippets
                                      . mkCompl ideOpts . stripAutoGenerated) filtCompls
                            ++ filtKeywordCompls
  
  return result

-- The supported languages and extensions
languagesAndExts :: [T.Text]
#if MIN_GHC_API_VERSION(8,10,0)
languagesAndExts = map T.pack $ DynFlags.supportedLanguagesAndExtensions ( PlatformMini ArchUnknown OSUnknown )
#else
languagesAndExts = map T.pack DynFlags.supportedLanguagesAndExtensions
#endif
  
-- ---------------------------------------------------------------------
-- helper functions for pragmas
-- ---------------------------------------------------------------------

validPragmas :: [(T.Text, T.Text)]
validPragmas =
  [ ("LANGUAGE ${1:extension}"        , "LANGUAGE")
  , ("OPTIONS_GHC -${1:option}"       , "OPTIONS_GHC")
  , ("INLINE ${1:function}"           , "INLINE")
  , ("NOINLINE ${1:function}"         , "NOINLINE")
  , ("INLINABLE ${1:function}"        , "INLINABLE")
  , ("WARNING ${1:message}"           , "WARNING")
  , ("DEPRECATED ${1:message}"        , "DEPRECATED")
  , ("ANN ${1:annotation}"            , "ANN")
  , ("RULES"                          , "RULES")
  , ("SPECIALIZE ${1:function}"       , "SPECIALIZE")
  , ("SPECIALIZE INLINE ${1:function}", "SPECIALIZE INLINE")
  ]

pragmaSuffix :: T.Text -> T.Text
pragmaSuffix fullLine
  |  "}" `T.isSuffixOf` fullLine = mempty
  | otherwise = " #-}"

-- ---------------------------------------------------------------------
-- helper functions for infix backticks
-- ---------------------------------------------------------------------

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


-- ---------------------------------------------------------------------

-- | Under certain circumstance GHC generates some extra stuff that we
-- don't want in the autocompleted symbols
stripAutoGenerated :: CompItem -> CompItem
stripAutoGenerated ci =
    ci {label = stripPrefix (label ci)}
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

keywords :: [T.Text]
keywords =
  [
    -- From https://wiki.haskell.org/Keywords
    "as"
  , "case", "of"
  , "class", "instance", "type"
  , "data", "family", "newtype"
  , "default"
  , "deriving"
  , "do", "mdo", "proc", "rec"
  , "forall"
  , "foreign"
  , "hiding"
  , "if", "then", "else"
  , "import", "qualified", "hiding"
  , "infix", "infixl", "infixr"
  , "let", "in", "where"
  , "module"
  ]

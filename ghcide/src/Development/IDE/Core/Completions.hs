-- Mostly taken from "haskell-ide-engine"
module Development.IDE.Core.Completions (
  CachedCompletions
, cacheDataProducer
, WithSnippets(..)
,getCompletions
) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Char (isSpace)
import Data.Generics
import Data.List as List hiding (stripPrefix)
import qualified Data.Map  as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Text.Fuzzy as Fuzzy

import GHC
import Module
import HscTypes
import Name
import RdrName
import TcRnTypes
import Type
import Var
import Packages
import DynFlags
import ConLike
import DataCon
import SrcLoc as GHC

import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Capabilities
import qualified Language.Haskell.LSP.VFS as VFS
import Development.IDE.Core.CompletionsTypes
import Development.IDE.Spans.Documentation

-- From haskell-ide-engine/src/Haskell/Ide/Engine/Support/HieExtras.hs

data NameDetails
  = NameDetails Module OccName
  deriving (Eq)

nsJSON :: NameSpace -> Value
nsJSON ns
  | isVarNameSpace ns = String "v"
  | isDataConNameSpace ns = String "c"
  | isTcClsNameSpace ns  = String "t"
  | isTvNameSpace ns = String "z"
  | otherwise = error "namespace not recognized"

parseNs :: Value -> Parser NameSpace
parseNs (String "v") = pure Name.varName
parseNs (String "c") = pure dataName
parseNs (String "t") = pure tcClsName
parseNs (String "z") = pure tvName
parseNs _ = mempty

instance FromJSON NameDetails where
  parseJSON v@(Array _)
    = do
      [modname,modid,namesp,occname] <- parseJSON v
      mn  <- parseJSON modname
      mid <- parseJSON modid
      ns <- parseNs namesp
      occn <- parseJSON occname
      pure $ NameDetails (mkModule (stringToUnitId mid) (mkModuleName mn)) (mkOccName ns occn)
  parseJSON _ = mempty
instance ToJSON NameDetails where
  toJSON (NameDetails mdl occ) = toJSON [toJSON mname,toJSON mid,nsJSON ns,toJSON occs]
    where
      mname = moduleNameString $ moduleName mdl
      mid = unitIdString $ moduleUnitId mdl
      ns = occNameSpace occ
      occs = occNameString occ

safeTyThingId :: TyThing -> Maybe Id
safeTyThingId (AnId i)                    = Just i
safeTyThingId (AConLike (RealDataCon dc)) = Just $ dataConWrapId dc
safeTyThingId _                           = Nothing

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
  | Just (L (RealSrcSpan r) modName) <- moduleHeader
  , pos `isInsideRange` r
  = Just (ModuleContext (moduleNameString modName))

  | Just (L (RealSrcSpan r) _) <- exportList
  , pos `isInsideRange` r
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
        go (L (RealSrcSpan r) SigD {})
          | pos `isInsideRange` r = Just TypeContext
          | otherwise = Nothing
        go (L (GHC.RealSrcSpan r) GHC.ValD {})
          | pos `isInsideRange` r = Just ValueContext
          | otherwise = Nothing
        go _ = Nothing

        goInline :: GHC.LHsType GhcPs -> Maybe Context
        goInline (GHC.L (GHC.RealSrcSpan r) _)
          | pos `isInsideRange` r = Just TypeContext
          | otherwise = Nothing
        goInline _ = Nothing

        p `isInsideRange` r = sp <= p && p <= ep
          where (sp, ep) = unpackRealSrcSpan r

        -- | Converts from one based tuple
        toPos :: (Int,Int) -> Position
        toPos (l,c) = Position (l-1) (c-1)
          
        unpackRealSrcSpan :: GHC.RealSrcSpan -> (Position, Position)
        unpackRealSrcSpan rspan =
          (toPos (l1,c1),toPos (l2,c2))
          where s  = GHC.realSrcSpanStart rspan
                l1 = GHC.srcLocLine s
                c1 = GHC.srcLocCol s
                e  = GHC.realSrcSpanEnd rspan
                l2 = GHC.srcLocLine e
                c2 = GHC.srcLocCol e

        importGo :: GHC.LImportDecl GhcPs -> Maybe Context
        importGo (L (RealSrcSpan r) impDecl)
          | pos `isInsideRange` r
          = importInline importModuleName (ideclHiding impDecl)
          <|> Just (ImportContext importModuleName)

          | otherwise = Nothing
          where importModuleName = moduleNameString $ unLoc $ ideclName impDecl

        importGo _ = Nothing

        importInline :: String -> Maybe (Bool,  GHC.Located [LIE GhcPs]) -> Maybe Context
        importInline modName (Just (True, L (RealSrcSpan r) _))
          | pos `isInsideRange` r = Just $ ImportHidingContext modName
          | otherwise = Nothing
        importInline modName (Just (False, L (RealSrcSpan r) _))
          | pos `isInsideRange` r = Just $ ImportListContext modName
          | otherwise = Nothing
        importInline _ _ = Nothing

type CompItemResolveData
  = Maybe NameDetails

occNameToComKind :: OccName -> CompletionItemKind
occNameToComKind oc
  | isVarOcc  oc = CiFunction
  | isTcOcc   oc = CiClass
  | isDataOcc oc = CiConstructor
  | otherwise    = CiVariable

mkCompl :: CompItem -> CompletionItem
mkCompl CI{origName,importedFrom,thingType,label,isInfix,docs} =
  CompletionItem label kind (Just $ maybe "" (<>"\n") typeText <> importedFrom)
    (Just $ CompletionDocMarkup $ MarkupContent MkMarkdown $ T.intercalate sectionSeparator docs)
    Nothing Nothing Nothing Nothing (Just insertText) (Just Snippet)
    Nothing Nothing Nothing Nothing resolveData
  where kind = Just $ occNameToComKind $ occName origName
        resolveData = Just (toJSON nameDets)
        insertText = case isInfix of
            Nothing -> case getArgText <$> thingType of
                            Nothing -> label
                            Just argText -> label <> " " <> argText
            Just LeftSide -> label <> "`"

            Just Surrounded -> label
        typeText
          | Just t <- thingType = Just . stripForall $ T.pack (showGhc t)
          | otherwise = Nothing
        nameDets =
          case (thingType, nameModule_maybe origName) of
            (Just _,_) -> Nothing
            (Nothing, Nothing) -> Nothing
            (Nothing, Just mdl) -> Just (NameDetails mdl (nameOccName origName))

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
      | isCoercionTy t = maybe [] (getArgs . snd) (splitCoercionType_maybe t)
      | otherwise = []

mkModCompl :: T.Text -> CompletionItem
mkModCompl label =
  CompletionItem label (Just CiModule) Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing (Just $ toJSON resolveData)
  where resolveData :: CompItemResolveData
        resolveData = Nothing

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

cacheDataProducer :: DynFlags -> TypecheckedModule -> [TypecheckedModule] -> IO CachedCompletions
cacheDataProducer dflags tm tcs = do
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

      getCompls :: [GlobalRdrElt] -> ([CompItem],QualCompls)
      getCompls = foldMap getComplsForOne

      getComplsForOne :: GlobalRdrElt -> ([CompItem],QualCompls)
      getComplsForOne (GRE n _ True _) =
        case lookupTypeEnv typeEnv n of
          Just tt -> case safeTyThingId tt of
            Just var -> ([varToCompl var],mempty)
            Nothing -> ([toCompItem curMod n],mempty)
          Nothing -> ([toCompItem curMod n],mempty)
      getComplsForOne (GRE n _ False prov) =
        flip foldMap (map is_decl prov) $ \spec ->
          let unqual
                | is_qual spec = []
                | otherwise = compItem
              qual
                | is_qual spec = Map.singleton asMod compItem
                | otherwise = Map.fromList [(asMod,compItem),(origMod,compItem)]
              compItem = [toCompItem (is_mod spec) n]
              asMod = showModName (is_as spec)
              origMod = showModName (is_mod spec)
          in (unqual,QualCompls qual)

      varToCompl :: Var -> CompItem
      varToCompl var = CI name (showModName curMod) typ label Nothing docs
        where
          typ = Just $ varType var
          name = Var.varName var
          label = T.pack $ showGhc name
          docs = getDocumentation tcs name

      toCompItem :: ModuleName -> Name -> CompItem
      toCompItem mn n =
        CI n (showModName mn) Nothing (T.pack $ showGhc n) Nothing (getDocumentation tcs n)

      (unquals,quals) = getCompls rdrElts

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
getCompletions :: CachedCompletions -> TypecheckedModule -> VFS.PosPrefixInfo -> ClientCapabilities -> WithSnippets -> IO [CompletionItem]
getCompletions CC { allModNamesAsNS, unqualCompls, qualCompls, importableModules }
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
                                      . mkCompl . stripAutoGenerated) filtCompls
  
  return result

-- The supported languages and extensions
languagesAndExts :: [T.Text]
languagesAndExts = map T.pack DynFlags.supportedLanguagesAndExtensions
  
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
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
  , pluginCompletionProvider = Just completion
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

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE CPP #-}
#include "ghc-api-version.h"

module Ide.Plugin.RecordSnippets
  (
    descriptor
  ) where

--import Control.Applicative
--import Control.Monad.Trans.Except (runExceptT)
--import qualified Language.Haskell.LSP.VFS as VFS
--import GHC
--import Data.Maybe (catMaybes)
--import Language.Haskell.GHC.ExactPrint.Utils (showGhc)
--import Development.IDE.Core.Service
--import qualified Parser
--import StringBuffer as SB
--import qualified Data.Maybe as UnsafeMaybe (fromJust)
--import qualified Data.Map as Map
--import Var
--import Development.IDE.Import.DependencyInformation (transitiveModuleDeps, TransitiveDependencies(TransitiveDependencies))
--import Development.IDE.Core.PositionMapping (PositionMapping)

import Data.Maybe
import qualified Data.Text as T
import Development.IDE as D
import Ide.Types
import Language.Haskell.LSP.Types
import Text.Regex.TDFA.Text()
import qualified Language.Haskell.LSP.Core as LSP
import Development.IDE.GHC.Compat
import GhcPlugins (
    liftIO,
    flLabel,
    unpackFS,
    occName)
import Data.List (intercalate)
import RdrName
import TcRnTypes
import HscTypes (HscEnv (hsc_dflags))
import Data.Functor ((<&>))
import Development.IDE.Core.Shake
import Development.IDE.Types.Options (IdeDefer(..))
import Control.Monad.Trans.Except
import qualified Development.IDE.Core.Compile as Compile
import Development.IDE.Spans.Common
import ConLike
import Data.Data
import GHC.Generics
import Data.Hashable
import Control.DeepSeq (NFData)
import Data.Binary (Binary)

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor
descriptor plId = (defaultPluginDescriptor plId)
  { pluginRules = produceRecordSnippets
  , pluginCommands = []
  , pluginCodeActionProvider = Nothing
  , pluginCodeLensProvider   = Nothing
  , pluginHoverProvider      = Nothing
  , pluginSymbolsProvider    = Nothing
  , pluginCompletionProvider = Just getSnippets
  }

type CachedSnippets = [(String, [(String, String)])]

-- | Produce completions info for a file
type instance RuleResult ProduceRecordSnippets = CachedSnippets
type instance RuleResult LocalSnippets = CachedSnippets
type instance RuleResult NonLocalSnippets = CachedSnippets

data ProduceRecordSnippets = ProduceRecordSnippets
    deriving (Eq, Show, Typeable, Generic)
instance Hashable ProduceRecordSnippets
instance NFData   ProduceRecordSnippets
instance Binary   ProduceRecordSnippets

data LocalSnippets = LocalSnippets
    deriving (Eq, Show, Typeable, Generic)
instance Hashable LocalSnippets
instance NFData   LocalSnippets
instance Binary   LocalSnippets

data NonLocalSnippets = NonLocalSnippets
    deriving (Eq, Show, Typeable, Generic)
instance Hashable NonLocalSnippets
instance NFData   NonLocalSnippets
instance Binary   NonLocalSnippets

produceRecordSnippets :: Rules ()
produceRecordSnippets = do
    define $ \ProduceRecordSnippets file -> do
        local <- useWithStale LocalSnippets file
        nonLocal <- useWithStale NonLocalSnippets file
        let extract = fmap fst
        return ([], extract local <> extract nonLocal)
    define $ \LocalSnippets file -> do
        local <- getLocalSnippets file
        return ([], local)
    define $ \NonLocalSnippets file -> do
        nonLocal <- getNonLocalSnippets file
        return ([], nonLocal)

getNonLocalSnippets :: NormalizedFilePath -> Action (Maybe CachedSnippets)
getNonLocalSnippets file = do
    -- Adopted from ghcide Development.IDE..Plugins.Completions
    ms <- fmap fst <$> useWithStale GetModSummaryWithoutTimestamps file
    sess <- fmap fst <$> useWithStale GhcSessionDeps file
    --deps <- maybe (TransitiveDependencies [] [] []) fst <$> useWithStale GetDependencies file
    --parsedDeps <- mapMaybe (fmap fst) <$> usesWithStale GetParsedModule (transitiveModuleDeps deps)
    case (ms, sess) of
        (Just ms, Just sess) -> do
            -- After parsing the module remove all package imports referring to
            -- these packages as we have already dealt with what they map to.
            let env = hscEnv sess
                buf = fromJust $ ms_hspp_buf ms
                f = fromNormalizedFilePath file
                dflags = hsc_dflags env
            pm <- liftIO $ evalGhcEnv env $ runExceptT $ Compile.parseHeader dflags f buf
            case pm of
                Right (_diags, hsMod) -> do
                    let hsModNoExports = hsMod <&> \x -> x{hsmodExports = Nothing}
                        pm = ParsedModule
                             { pm_mod_summary = ms
                             , pm_parsed_source = hsModNoExports
                             , pm_extra_src_files = [] -- src imports not allowed
                             , pm_annotations = mempty
                             }
                    tm <- liftIO $ Compile.typecheckModule (IdeDefer True) env pm
                    extras <- getShakeExtras
                    liftIO $ logInfo (logger extras) $ "----Non Local Snippet Called---"
                    case tm of
                        (_, Just (_,TcModuleResult{..})) -> do
                            cdata <- liftIO $ cachedSnippetsProducer extras env tmrModule
                            -- Do not return diags from parsing as they would duplicate
                            -- the diagnostics from typechecking
                            --return ([], Just cdata)
                            return $ Just cdata
                        (_diag, _) ->
                            return Nothing
                Left _diag -> return Nothing
        _ -> return Nothing


cachedSnippetsProducer :: ShakeExtras -> HscEnv -> TypecheckedModule -> IO [(String, [(String, String)])]
cachedSnippetsProducer ShakeExtras{logger} packageState tm = do
  let parsedMod = tm_parsed_module tm
      curMod = ms_mod $ pm_mod_summary parsedMod

      rdrEnv = tcg_rdr_env $ fst $ tm_internals_ tm
      rdrElts = globalRdrEnvElts rdrEnv

      foldMapM :: (Foldable f, Monad m, Monoid b) => (a -> m b) -> f a -> m b
      foldMapM f xs = foldr step return xs mempty where
        step x r z = f x >>= \y -> r $! z `mappend` y

      getCompls :: [GlobalRdrElt] -> IO [(String, [(String, String)])]
      getCompls = foldMapM getComplsForOne

      getComplsForOne :: GlobalRdrElt -> IO [(String, [(String, String)])]
      getComplsForOne (GRE _ _ True _) = return [] -- this should be covered in LocalCompletions
      getComplsForOne (GRE n _ False prov) =
        flip foldMapM (map is_decl prov) $ \_spec -> do
          --logInfo logger $ T.pack $ "*******Resolving prov***************************"
          --logInfo logger $ T.pack $ (showGhc prov)
          compItem <- toCompItem curMod n
          --logInfo logger $ T.pack $ show (compItem)
          case compItem of
              Just match -> do
                  logInfo logger $ T.pack $ show (match)
                  return [match]
              Nothing -> return []

      toCompItem :: Module -> Name -> IO (Maybe (String , [(String, String)]))
      toCompItem m n = do
          flds <- evalGhcEnv packageState $ catchSrcErrors "completion" $ do
              name' <- Compile.lookupName m n
              --liftIO $ logInfo logger $ T.pack $ "here"
              --liftIO $ logInfo logger $ T.pack $ showGhc name'
              return $ name' >>= safeTyThing_
          return $ (either (const Nothing) id flds)
  compls <- getCompls rdrElts
  return $ compls

safeTyThing_ :: TyThing -> Maybe (String, [(String, String)])
safeTyThing_ (AnId _) = Nothing
safeTyThing_ (AConLike dc) =
    let flds = conLikeFieldLabels $ dc
        name = occName . conLikeName $ dc
        types = map ((conLikeFieldType dc) . (flLabel)) flds
    in
        Just $ (showGhc name, [(unpackFS . flLabel $ x, showGhc y) | x <- flds | y <- types])
safeTyThing_ _ = Nothing


getSnippets
    :: LSP.LspFuncs cofd
    -> IdeState
    -> CompletionParams
    -> IO (Either ResponseError CompletionResponseResult)
getSnippets lsp ide
  CompletionParams{_textDocument=TextDocumentIdentifier uri
                  ,_position=_position
                  ,_context=_completionContext} = do
    contents <- LSP.getVirtualFileFunc lsp $ toNormalizedUri uri
    fmap Right $ case (contents, uriToFilePath' uri) of
      (Just _cnts, Just path) -> do
        let npath = toNormalizedFilePath' path
        all_snippets <- runIdeAction "RecordSnippetsNonLocal" (shakeExtras ide) $ do
            -- opts <- liftIO $ getIdeOptionsIO $ shakeExtras ide
            -- result <- getNonLocalSnippets npath
            result <- useWithStaleFast ProduceRecordSnippets npath
            case result of
                Just (snippets, _) -> do
                    liftIO $ logInfo (ideLogger ide) $ T.pack $ "Snippets show here ------->"
                    liftIO $ logInfo (ideLogger ide) $ T.pack $ show snippets
                    let compls = buildCompletions snippets
                    liftIO $ logInfo (ideLogger ide) $ T.pack $ show compls
                    pure compls
                Nothing -> return (Completions $ List [])
        return $ all_snippets
      _ -> return (Completions $ List [])


--- Gather local completions

getLocalSnippets :: NormalizedFilePath -> Action (Maybe CachedSnippets)
getLocalSnippets file = do
    pm <- useWithStale GetParsedModule file
    extras <- getShakeExtras
    case pm of
      Just (parsedMod, _) -> do
          snippets <- liftIO $ cachedLocalSnippetProducer extras parsedMod
          return $ Just snippets
      _ -> return Nothing


cachedLocalSnippetProducer :: ShakeExtras -> ParsedModule -> IO CachedSnippets
cachedLocalSnippetProducer ShakeExtras{logger} pmod = do
  let _hsmodule = unLoc (parsedSource pmod)
      hsDecls = hsmodDecls _hsmodule
      --ctxStr = (T.unpack . VFS.prefixText $ pfix)
      completionData = findFields (unLoc <$> hsDecls)
      -- compls = buildCompletions completionData
  logInfo logger $ "*****Showing Completion Data From Local Snippets****\n"
  logInfo logger $ T.pack $ show completionData
  return completionData


findFields :: [HsDecl GhcPs] -> CachedSnippets
findFields decls = name_type''
  where
    dataDefns = catMaybes $ findDataDefns <$> decls
    findDataDefns decl =
      case decl of
        TyClD _ (DataDecl{tcdDataDefn}) -> Just tcdDataDefn
        _ -> Nothing
    conDecls = concat [ unLoc <$> dd_cons dataDefn | dataDefn <- dataDefns]
    h98 = catMaybes $ findH98 <$> conDecls
    findH98 conDecl = case conDecl of
      ConDeclH98{..} -> Just (unLoc con_name, con_args)
      ConDeclGADT{} -> Nothing  -- TODO: Expand this out later
      _ -> Nothing

    name_flds :: [(RdrName, [ConDeclField GhcPs])]
    name_flds = catMaybes $ decompose <$> h98

    decompose x = case getFlds $ (snd x) of
                      Just con_details -> Just (fst x, con_details)
                      Nothing -> Nothing

    getFlds :: HsConDetails arg (Located [LConDeclField GhcPs]) -> Maybe [SrcSpanLess (LConDeclField GhcPs)]
    getFlds conArg = case conArg of
                         RecCon rec -> Just $ unLoc <$> (unLoc rec)
                         _ -> Nothing

    --name_type :: [(RdrName, (Located RdrName, HsType GhcPs))]

    name_type = decompose' <$> name_flds
    decompose' x = (fst x, catMaybes $ extract <$> (snd x))

    extract ConDeclField{..} = let
        fld_type = unLoc cd_fld_type
        fld_name = rdrNameFieldOcc $ unLoc . head $ cd_fld_names --TODO: Why is cd_fld_names a list?
        in
            Just (fld_name, fld_type)
    extract _ = Nothing

    name_type'' = decompose'' <$> name_type
    decompose'' :: (RdrName, [(Located RdrName, HsType GhcPs)]) -> (String, [(String, String)])
    decompose'' x = (showGhc . fst $ x,
                     (\(x', y') -> (showGhc . unLoc $ x', showGhc y'))  <$> (snd x)
                    )


--- Functions that build the final code snippets
buildCompletionItem :: String -> [(String, String)] -> CompletionItem
buildCompletionItem ctxStr completionData = r
  where
    r =
      CompletionItem
        label
        kind
        tags
        detail
        documentation
        deprecated
        preselect
        sortText
        filterText
        insertText
        insertTextFormat
        textEdit
        additionalTextEdits
        commitCharacters
        command
        xd
    label = T.pack ctxStr
    kind = Just CiSnippet
    tags = List []
    detail = Nothing
    documentation = Nothing
    deprecated = Nothing
    preselect = Nothing
    sortText = Nothing
    filterText = Nothing
    insertText = Just $ buildSnippet
    insertTextFormat = Just Snippet
    textEdit = Nothing
    additionalTextEdits = Nothing
    commitCharacters = Nothing
    command = Nothing
    xd = Nothing

    t = zip completionData ([1..]::[Int])
    snippet = intercalate ", " $
        map (\(x, i) -> ((fst x) <> "=${" <> show i <> ":" <> (fst x) <> "}")) t
    buildSnippet = T.pack $ ctxStr <> " {" <> snippet <> "}"


buildCompletions :: CachedSnippets -> CompletionResponseResult
buildCompletions snippets = let
    result = (uncurry buildCompletionItem) <$> snippets
    in
    Completions $ List result


-----------------------------------------

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
-- TODO1 added by Example Plugin directly
-- TODO1 added by Example Plugin directly
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

import Control.Applicative
import Control.Monad.Trans.Except (runExceptT)
import Data.Maybe
import qualified Data.Text as T
import Development.IDE as D
import Ide.Types
import Language.Haskell.LSP.Types
import Text.Regex.TDFA.Text()
import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.VFS as VFS
import Development.IDE.GHC.Compat
import GHC
import Data.Maybe (catMaybes)
import GhcPlugins (occNameString, rdrNameOcc, GlobalRdrElt(..), liftIO, listVisibleModuleNames)
--import Language.Haskell.GHC.ExactPrint.Utils (showGhc)
import Data.List (intercalate)
import RdrName
import TcRnTypes
import HscTypes (HscEnv (hsc_dflags), lookupTypeEnv)
import Data.Functor ((<&>))
import Development.IDE.Core.Shake
--import Development.IDE.Core.Service
import Development.IDE.Types.Options (IdeDefer(..))
import qualified Parser
import Control.Monad.Trans.Except
import StringBuffer as SB
import qualified Development.IDE.Core.Compile as Compile
import qualified Data.Maybe as UnsafeMaybe (fromJust)
import qualified Data.Map as Map
import Var
import Development.IDE.Spans.Common
import Development.IDE.Import.DependencyInformation (transitiveModuleDeps, TransitiveDependencies(TransitiveDependencies))
--import Development.IDE.Plugin.Completions

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor
descriptor plId = (defaultPluginDescriptor plId)
  { pluginRules = mempty
  , pluginCommands = []
  , pluginCodeActionProvider = Nothing
  , pluginCodeLensProvider   = Nothing
  , pluginHoverProvider      = Nothing
  , pluginSymbolsProvider    = Nothing
  , pluginCompletionProvider = Just getNonLocalCompletionsLSP
  }

-- ---------------------------------------------------------------------

-- -- | Parse only the module header taben from GHCIDE.Compile.
-- -- | We could move this to Util maybe
-- parseHeader
--        :: GhcMonad m
--        => DynFlags -- ^ flags to use
--        -> FilePath  -- ^ the filename (for source locations)
--        -> SB.StringBuffer -- ^ Haskell module source text (full Unicode is supported)
--        -> ExceptT [FileDiagnostic] m ([FileDiagnostic], Located(HsModule GhcPs))
-- parseHeader dflags filename contents = do
--    let loc  = CompilmkRealSrcLoc (mkFastString filename) 1 1
--    case unP Parser.parseHeader (mkPState dflags contents loc) of
-- #if MIN_GHC_API_VERSION(8,10,0)
--      PFailed pst ->
--         throwE $ diagFromErrMsgs "parser" dflags $ getErrorMessages pst dflags
-- #else
--      PFailed _ locErr msgErr ->
--         throwE $ diagFromErrMsg "parser" dflags $ mkPlainErrMsg dflags locErr msgErr
-- #endif
--      POk pst rdr_module -> do
--         let (warns, errs) = getMessages pst dflags
--         -- Just because we got a `POk`, it doesn't mean there
--         -- weren't errors! To clarify, the GHC parser
--         -- distinguishes between fatal and non-fatal
--         -- errors. Non-fatal errors are the sort that don't
--         -- prevent parsing from continuing (that is, a parse
--         -- tree can still be produced despite the error so that
--         -- further errors/warnings can be collected). Fatal
--         -- errors are those from which a parse tree just can't
--         -- be produced.
--         unless (null errs) $
--             throwE $ diagFromErrMsgs "parser" dflags errs

--         let warnings = diagFromErrMsgs "parser" dflags warns
--         return (warnings, rdr_module)

getTypeCheckModule _lsp ide _uri = do
    contents <- liftIO $ LSP.getVirtualFileFunc _lsp $ toNormalizedUri _uri
    fmap Right $ case (contents, uriToFilePath' _uri) of
        (Just _cnts, Just _path) -> do
            let file = toNormalizedFilePath' _path
            --pm' <- runIdeAction "RecordsSnippets" (shakeExtras ide) $ do

            ms <- fmap fst <$> useWithStale GetModSummaryWithoutTimestamps file
            sess <- fmap fst <$> useWithStale GhcSessionDeps file
            _deps <- maybe (TransitiveDependencies [] [] []) fst <$> useWithStale GetDependencies file
            parsedDeps <- mapMaybe (fmap fst) <$> usesWithStale GetParsedModule (transitiveModuleDeps _deps)
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
                            case tm of
                                (_, Just (_,TcModuleResult{..})) -> do
                                    cdata <- liftIO $ cacheDataProducer ide env tmrModule parsedDeps
                                    -- Do not return diags from parsing as they would duplicate
                                    -- the diagnostics from typechecking
                                    --return ([], Just cdata)
                                    return ([], Nothing)
                                (_diag, _) ->
                                    return ([], Nothing)
                        Left _diag -> return ([], Nothing)
                _ -> return ([], Nothing)
        _ -> return ([], Nothing)


showModName :: ModuleName -> T.Text
showModName = T.pack . moduleNameString

cacheDataProducer :: IdeState -> HscEnv -> TypecheckedModule -> [ParsedModule] -> IO [(String, String)]
cacheDataProducer ide packageState tm deps = do
  let parsedMod = tm_parsed_module tm
      dflags = hsc_dflags packageState
      curMod = ms_mod $ pm_mod_summary parsedMod
      curModName = moduleName curMod
      (_,limports,_,_) = UnsafeMaybe.fromJust $ tm_renamed_source tm -- safe because we always save the typechecked source

      iDeclToModName :: ImportDecl name -> ModuleName
      iDeclToModName = unLoc . ideclName

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

      getCompls :: [GlobalRdrElt] -> IO [(String, String)]
      getCompls = foldMapM getComplsForOne

      getComplsForOne :: GlobalRdrElt -> IO [(String, String)]
      getComplsForOne (GRE n _ True _) =
        case lookupTypeEnv typeEnv n of
          Just tt -> case safeTyThingId tt of
            Just var -> do
                logInfo (ideLogger ide) $ T.pack $ "*******Resolving tt***************************"
                logInfo (ideLogger ide) $ T.pack $ (showGhc tt)
                (\x -> [x]) <$> varToCompl var
            Nothing -> return [] -- (\x -> ([x],mempty)) <$> toCompItem curMod curModName n
          Nothing -> return [] -- (\x -> ([x],mempty)) <$> toCompItem curMod curModName n
      getComplsForOne (GRE n _ False prov) =
        flip foldMapM (map is_decl prov) $ \spec -> do
          compItem <- toCompItem curMod (is_mod spec) n
          let unqual
                | is_qual spec = []
                | otherwise = [] -- [compItem]
              qual
                | is_qual spec = Map.singleton asMod [compItem]
                | otherwise = Map.fromList [(asMod,[compItem]),(origMod,[compItem])]
              asMod = showModName (is_as spec)
              origMod = showModName (is_mod spec)
          return []

      varToCompl :: Var -> IO (String, String)
      varToCompl var = do
        let typ = Just $ varType var
            name = Var.varName var
        return (showGhc name, showGhc typ)
        --docs <- evalGhcEnv packageState $ getDocumentationTryGhc curMod (tm_parsed_module tm : deps) name
        --return $ mkNameCompItem name curModName typ Nothing docs

      toCompItem :: Module -> ModuleName -> Name -> IO [(String, String)]
      toCompItem m mn n = do
        --docs <- evalGhcEnv packageState $ getDocumentationTryGhc curMod (tm_parsed_module tm : deps) n
        ty <- evalGhcEnv packageState $ catchSrcErrors "completion" $ do
                name' <- Compile.lookupName m n
                return $ name' >>= safeTyThingType
        return []
        --return $ mkNameCompItem n mn (either (const Nothing) id ty) Nothing docs

  compls <- getCompls rdrElts
  return $ compls


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
        pm <- runIdeAction "RecordsSnippets" (shakeExtras ide) $ do
            --opts <- liftIO $ getIdeOptionsIO $ shakeExtras ide
            --compls <- useWithStaleFast ProduceCompletions npath
            pm <- useWithStaleFast GetParsedModule npath
            --binds <- fromMaybe (mempty, zeroMapping) <$> useWithStaleFast GetBindings npath
            pure pm
        case pm of
          Just (parsedMod, _) -> do
            pfix <- VFS.getCompletionPrefix position cnts
            case (pfix, completionContext) of
              (Just (VFS.PosPrefixInfo _ "" _ _), Just CompletionContext { _triggerCharacter = Just "."})
                -> return (Completions $ List [])
              (Just pfix', _) -> do
                  -- TODO pass the real capabilities here (or remove the logic for snippets)
                --let fakeClientCapabilities = ClientCapabilities Nothing Nothing Nothing Nothing
                --Completions . List <$> getCompletions ideOpts cci' parsedMod bindMap pfix' fakeClientCapabilities (WithSnippets True)
                  logInfo (ideLogger ide) $ T.pack $ "**********************************"
                  logInfo (ideLogger ide) $ T.pack $ show completionContext
                  logInfo (ideLogger ide) $ T.pack $ show pfix'
                  findLocalCompletions ide parsedMod pfix'
              _ -> return (Completions $ List [])
          _ -> return (Completions $ List [])
      _ -> return (Completions $ List [])


getNonLocalCompletionsLSP
    :: LSP.LspFuncs cofd
    -> IdeState
    -> CompletionParams
    -> IO (Either ResponseError CompletionResponseResult)
getNonLocalCompletionsLSP lsp ide
  CompletionParams{_textDocument=TextDocumentIdentifier uri
                  ,_position=position
                  ,_context=completionContext} = do
    contents <- LSP.getVirtualFileFunc lsp $ toNormalizedUri uri
    fmap Right $ case (contents, uriToFilePath' uri) of
      (Just cnts, Just path) -> do
        let npath = toNormalizedFilePath' path
        pm <- runAction "RecordsSnippets" ide $ do
            --opts <- liftIO $ getIdeOptionsIO $ shakeExtras ide
            --compls <- useWithStaleFast ProduceCompletions npath
              x <- getTypeCheckModule lsp ide uri
              pure x
        return (Completions $ List [])
      _ -> return (Completions $ List [])


findLocalCompletions :: IdeState -> ParsedModule -> VFS.PosPrefixInfo -> IO CompletionResponseResult
findLocalCompletions ide  pmod pfix = do
    let _hsmodule = unLoc (parsedSource pmod)
        hsDecls = hsmodDecls _hsmodule
        ctxStr = (T.unpack . VFS.prefixText $ pfix)
        completionData = findFields ctxStr (unLoc <$> hsDecls)
    x <- buildCompletion ctxStr completionData
    logInfo (ideLogger ide) $ "*****Showing Completion Data\n"
    logInfo (ideLogger ide) $ T.pack $ show completionData
    return x


findFields :: String -> [HsDecl GhcPs] -> [(String, String)]
findFields  ctxStr decls = name_type
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

    conArgs = [snd x | x  <- h98, (occNameString . rdrNameOcc . fst $ x) == ctxStr]
    flds = concat . catMaybes $ getFlds <$> conArgs
    getFlds conArg = case conArg of
      RecCon rec -> Just $ unLoc <$> (unLoc rec)
      _ -> Nothing
    name_type = map (\x -> ((showGhc . fst $ x), (showGhc . snd $ x))) (catMaybes $ extract <$> flds)

    extract ConDeclField{..} = let
      fld_type = unLoc cd_fld_type
      fld_name = rdrNameFieldOcc $ unLoc . head $ cd_fld_names --TODO: Why is cd_fld_names a list?
        in
        Just (fld_name, fld_type)
    extract _ = Nothing


buildCompletion :: String -> [(String, String)] -> IO CompletionResponseResult
buildCompletion ctxStr completionData = do
  pure $ Completions $ List [r]
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

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

#include "ghc-api-version.h"

module Ide.Plugin.ImportLens (descriptor) where
import           Control.Monad                  (forM)
import           Data.Aeson                     (ToJSON)
import           Data.Aeson                     (Value (Null))
import           Data.Aeson                     (ToJSON (toJSON))
import           Data.Aeson.Types               (FromJSON)
import qualified Data.HashMap.Strict            as HashMap
import           Data.IORef                     (readIORef)
import           Data.Map                       (Map)
import qualified Data.Map.Strict                as Map
import           Data.Maybe                     (catMaybes, fromMaybe)
import qualified Data.Text                      as T
import           Development.IDE
import           Development.IDE.GHC.Compat
import           GHC.Generics                   (Generic)
import           Ide.Plugin
import           Ide.Types
import           Language.Haskell.LSP.Types
import           PrelNames                      (pRELUDE)
import           RnNames                        (findImportUsage,
                                                 getMinimalImports)
import           TcRnMonad                      (initTcWithGbl)
import           TcRnTypes                      (TcGblEnv (tcg_used_gres))

importCommandId :: CommandId
importCommandId = "ImportLensCommand"

-- | The "main" function of a plugin
descriptor :: PluginId -> PluginDescriptor
descriptor plId = (defaultPluginDescriptor plId) {
    -- This plugin provides code lenses
    pluginCodeLensProvider = Just provider,
    -- This plugin provides a command handler
    pluginCommands = [ importLensCommand ]
}

-- | The command descriptor
importLensCommand :: PluginCommand
importLensCommand =
    PluginCommand importCommandId "Explicit import command" runImportCommand

-- | The type of the parameters accepted by our command
data ImportCommandParams = ImportCommandParams WorkspaceEdit
  deriving Generic
  deriving anyclass (FromJSON, ToJSON)

-- | The actual command handler
runImportCommand :: CommandFunction ImportCommandParams
runImportCommand _lspFuncs _state (ImportCommandParams edit) = do
    -- This command simply triggers a workspace edit!
    return (Right Null, Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams edit))

-- | For every implicit import statement, return a code lens of the corresponding explicit import
-- Example - for the module below:
--
-- > import Data.List
-- >
-- > f = intercalate " " . sortBy length
--
-- the provider should produce one code lens associated to the import statement:
--
-- > import Data.List (intercalate, sortBy)
provider :: CodeLensProvider
provider _lspFuncs          -- LSP functions, not used
         state              -- ghcide state, used to retrieve typechecking artifacts
         pId                -- plugin Id
         CodeLensParams{_textDocument = TextDocumentIdentifier{_uri}}
  -- VSCode uses URIs instead of file paths
  -- haskell-lsp provides conversion functions
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri _uri
  = do
    -- Get the typechecking artifacts from the module
    tmr <- runIde state $ useWithStale TypeCheck nfp
    -- We also need a GHC session with all the dependencies
    hsc <- runIde state $ useWithStale GhcSessionDeps nfp
    -- Use the GHC api to extract the "minimal" imports
    (imports, mbMinImports) <- extractMinimalImports (fst <$> hsc) ( fst <$> tmr)

    case mbMinImports of
        -- Implement the provider logic:
        -- for every import, if it's lacking a explicit list, generate a code lens
        Just minImports -> do
            let minImportsMap =
                    Map.fromList [ (srcSpanStart l, i) | L l i <- minImports ]
            commands <- forM imports $ generateLens pId _uri minImportsMap
            return $ Right (List $ catMaybes commands)
        _ ->
            return $ Right (List [])

  | otherwise
  = return $ Right (List [])

-- | Use the ghc api to extract a minimal, explicit set of imports for this module
extractMinimalImports
  :: Maybe (HscEnvEq)
  -> Maybe (TcModuleResult)
  -> IO ([LImportDecl GhcRn], Maybe [LImportDecl GhcRn])
extractMinimalImports (Just (hsc)) (Just (tmrModule -> TypecheckedModule{..})) = do
    -- extract the original imports and the typechecking environment
    let (tcEnv,_) = tm_internals_
        Just (_, imports, _, _) = tm_renamed_source
        ParsedModule{ pm_parsed_source = L loc _} = tm_parsed_module
        span = fromMaybe (error "expected real") $ realSpan loc

    -- GHC is secretly full of mutable state
    gblElts <- readIORef (tcg_used_gres tcEnv)

    -- call findImportUsage does exactly what we need
    -- GHC is full of treats like this
    let usage = findImportUsage imports gblElts
    (_, minimalImports) <- initTcWithGbl (hscEnv hsc) tcEnv span $ getMinimalImports usage

    -- return both the original imports and the computed minimal ones
    return (imports, minimalImports)

extractMinimalImports _ _ = return ([], Nothing)

-- | Given an import declaration, generate a code lens unless it has an
-- explicit import list or it's qualified
generateLens :: PluginId -> Uri -> Map SrcLoc (ImportDecl GhcRn) -> LImportDecl GhcRn -> IO (Maybe CodeLens)
generateLens pId uri minImports (L src imp)
  -- Explicit import list case
  | ImportDecl{ideclHiding = Just (False,_)} <- imp
  = return Nothing
  -- Qualified case
  | isQualifiedImport imp
  = return Nothing
  -- No explicit import list
  | RealSrcSpan l <- src
  , Just explicit <- Map.lookup (srcSpanStart src) minImports
  , L _ mn <- ideclName imp
  -- (almost) no one wants to see an explicit import list for Prelude
  , mn /= moduleName pRELUDE
  = do
        -- The title of the command is just the minimal explicit import decl
    let title = T.pack $ prettyPrint explicit
        -- the range of the code lens is the span of the original import decl
        _range :: Range = realSrcSpanToRange l
        -- the code lens has no extra data
        _xdata = Nothing
        -- an edit that replaces the whole declaration with the explicit one
        edit = WorkspaceEdit (Just editsMap) Nothing
        editsMap = HashMap.fromList [(uri, List [importEdit])]
        importEdit = TextEdit _range title
        -- the command argument is simply the edit
        _arguments = Just [toJSON $ ImportCommandParams edit]
    -- create the command
    _command <- Just <$> mkLspCommand pId importCommandId title _arguments
    -- create and return the code lens
    return $ Just CodeLens{..}
  | otherwise
  = return Nothing

-- | A helper to run ide actions
runIde :: IdeState -> Action a -> IO a
runIde state = runAction "importLens" state

isQualifiedImport :: ImportDecl a -> Bool
#if MIN_GHC_API_VERSION(8,10,0)
isQualifiedImport ImportDecl{ideclQualified = NotQualified} = False
isQualifiedImport ImportDecl{} = True
#else
isQualifiedImport ImportDecl{ideclQualified} = ideclQualified
#endif
isQualifiedImport _ = False

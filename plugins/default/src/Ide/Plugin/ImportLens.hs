{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

#include "ghc-api-version.h"

module Ide.Plugin.ImportLens (descriptor) where

import Control.DeepSeq
import Control.Monad.IO.Class
import Data.Aeson (ToJSON (toJSON), Value (Null))
import Data.Aeson.Types (FromJSON)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import Development.IDE
import Development.IDE.Core.PositionMapping
import Development.IDE.GHC.Compat
import Development.Shake.Classes
import GHC.Generics (Generic)
import Ide.Plugin
import Ide.Types
import Language.Haskell.LSP.Types
import PrelNames (pRELUDE)
import RnNames
  ( findImportUsage,
    getMinimalImports,
  )
import TcRnMonad (initTcWithGbl)
import TcRnTypes (TcGblEnv (tcg_used_gres))

importCommandId :: CommandId
importCommandId = "ImportLensCommand"

-- | The "main" function of a plugin
descriptor :: PluginId -> PluginDescriptor
descriptor plId =
  (defaultPluginDescriptor plId)
    { -- This plugin provides code lenses
      pluginCodeLensProvider = Just lensProvider,
      -- This plugin provides a command handler
      pluginCommands = [importLensCommand],
      -- This plugin provides code actions
      pluginCodeActionProvider = Just codeActionProvider,
      -- This plugin defines a new rule
      pluginRules = minimalImportsRule
    }

-- | The command descriptor
importLensCommand :: PluginCommand
importLensCommand =
  PluginCommand importCommandId "Explicit import command" runImportCommand

-- | The type of the parameters accepted by our command
data ImportCommandParams = ImportCommandParams WorkspaceEdit
  deriving (Generic)
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
lensProvider :: CodeLensProvider
lensProvider
  _lspFuncs -- LSP functions, not used
  state -- ghcide state, used to retrieve typechecking artifacts
  pId -- plugin Id
  CodeLensParams {_textDocument = TextDocumentIdentifier {_uri}}
    -- VSCode uses URIs instead of file paths
    -- haskell-lsp provides conversion functions
    | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri _uri =
      do
        mbMinImports <- runAction "" state $ useWithStale MinimalImports nfp
        case mbMinImports of
          -- Implement the provider logic:
          -- for every import, if it's lacking a explicit list, generate a code lens
          Just (MinimalImportsResult minImports, posMapping) -> do
            commands <-
              sequence
                [ generateLens pId _uri edit
                  | (imp, Just minImport) <- minImports,
                    Just edit <- [mkExplicitEdit posMapping imp minImport]
                ]
            return $ Right (List $ catMaybes commands)
          _ ->
            return $ Right (List [])
    | otherwise =
      return $ Right (List [])

-- | If there are any implicit imports, provide one code action to turn them all
--   into explicit imports.
codeActionProvider :: CodeActionProvider
codeActionProvider _lspFuncs ideState _pId docId range _context
  | TextDocumentIdentifier {_uri} <- docId,
    Just nfp <- uriToNormalizedFilePath $ toNormalizedUri _uri =
    do
      pm <- runIde ideState $ use GetParsedModule nfp
      let insideImport = case pm of
            Just ParsedModule {pm_parsed_source}
              | locImports <- hsmodImports (unLoc pm_parsed_source),
                rangesImports <- map getLoc locImports ->
                any (within range) rangesImports
            _ -> False
      if not insideImport
        then return (Right (List []))
        else do
          minImports <- runAction "MinimalImports" ideState $ use MinimalImports nfp
          let edits =
                [ e
                  | (imp, Just explicit) <-
                      maybe [] getMinimalImportsResult minImports,
                    Just e <- [mkExplicitEdit zeroMapping imp explicit]
                ]
              caExplicitImports = CACodeAction CodeAction {..}
              _title = "Make all imports explicit"
              _kind = Just CodeActionQuickFix
              _command = Nothing
              _edit = Just WorkspaceEdit {_changes, _documentChanges}
              _changes = Just $ HashMap.singleton _uri $ List edits
              _documentChanges = Nothing
              _diagnostics = Nothing
          return $ Right $ List [caExplicitImports | not (null edits)]
  | otherwise =
    return $ Right $ List []

--------------------------------------------------------------------------------

data MinimalImports = MinimalImports
  deriving (Show, Generic, Eq, Ord)

instance Hashable MinimalImports

instance NFData MinimalImports

instance Binary MinimalImports

type instance RuleResult MinimalImports = MinimalImportsResult

newtype MinimalImportsResult = MinimalImportsResult
  {getMinimalImportsResult :: [(LImportDecl GhcRn, Maybe T.Text)]}

instance Show MinimalImportsResult where show _ = "<minimalImportsResult>"

instance NFData MinimalImportsResult where rnf = rwhnf

minimalImportsRule :: Rules ()
minimalImportsRule = define $ \MinimalImports nfp -> do
  -- Get the typechecking artifacts from the module
  tmr <- use TypeCheck nfp
  -- We also need a GHC session with all the dependencies
  hsc <- use GhcSessionDeps nfp
  -- Use the GHC api to extract the "minimal" imports
  (imports, mbMinImports) <- liftIO $ extractMinimalImports hsc tmr
  let importsMap =
        Map.fromList
          [ (srcSpanStart l, T.pack (prettyPrint i))
            | L l i <- fromMaybe [] mbMinImports
          ]
      res =
        [ (i, Map.lookup (srcSpanStart (getLoc i)) importsMap)
          | i <- imports
        ]
  return ([], MinimalImportsResult res <$ mbMinImports)

--------------------------------------------------------------------------------

-- | Use the ghc api to extract a minimal, explicit set of imports for this module
extractMinimalImports ::
  Maybe (HscEnvEq) ->
  Maybe (TcModuleResult) ->
  IO ([LImportDecl GhcRn], Maybe [LImportDecl GhcRn])
extractMinimalImports (Just (hsc)) (Just (tmrModule -> TypecheckedModule {..})) = do
  -- extract the original imports and the typechecking environment
  let (tcEnv, _) = tm_internals_
      Just (_, imports, _, _) = tm_renamed_source
      ParsedModule {pm_parsed_source = L loc _} = tm_parsed_module
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

mkExplicitEdit :: PositionMapping -> LImportDecl pass -> T.Text -> Maybe TextEdit
mkExplicitEdit posMapping (L src imp) explicit
  -- Explicit import list case
  | ImportDecl {ideclHiding = Just (False, _)} <- imp =
    Nothing
  | not (isQualifiedImport imp),
    RealSrcSpan l <- src,
    L _ mn <- ideclName imp,
    -- (almost) no one wants to see an explicit import list for Prelude
    mn /= moduleName pRELUDE,
    Just rng <- toCurrentRange posMapping $ realSrcSpanToRange l =
    Just $ TextEdit rng explicit
  | otherwise =
    Nothing

-- | Given an import declaration, generate a code lens unless it has an
-- explicit import list or it's qualified
generateLens :: PluginId -> Uri -> TextEdit -> IO (Maybe CodeLens)
generateLens pId uri importEdit@TextEdit {_range} = do
  -- The title of the command is just the minimal explicit import decl
  let title = _newText importEdit
      -- the code lens has no extra data
      _xdata = Nothing
      -- an edit that replaces the whole declaration with the explicit one
      edit = WorkspaceEdit (Just editsMap) Nothing
      editsMap = HashMap.fromList [(uri, List [importEdit])]
      -- the command argument is simply the edit
      _arguments = Just [toJSON $ ImportCommandParams edit]
  -- create the command
  _command <- Just <$> mkLspCommand pId importCommandId title _arguments
  -- create and return the code lens
  return $ Just CodeLens {..}

-- | A helper to run ide actions
runIde :: IdeState -> Action a -> IO a
runIde state = runAction "importLens" state

--------------------------------------------------------------------------------

isQualifiedImport :: ImportDecl a -> Bool
#if MIN_GHC_API_VERSION(8,10,0)
isQualifiedImport ImportDecl{ideclQualified = NotQualified} = False
isQualifiedImport ImportDecl{} = True
#else
isQualifiedImport ImportDecl{ideclQualified} = ideclQualified
#endif
isQualifiedImport _ = False

within :: Range -> SrcSpan -> Bool
within (Range start end) span =
  isInsideSrcSpan start span || isInsideSrcSpan end span

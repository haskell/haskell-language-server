{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

#include "ghc-api-version.h"

module Ide.Plugin.ExplicitImports (descriptor) where

import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Data.Aeson                           (ToJSON (toJSON),
                                                       Value (Null))
import           Data.Aeson.Types                     (FromJSON)
import qualified Data.HashMap.Strict                  as HashMap
import           Data.IORef                           (readIORef)
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (catMaybes, fromMaybe)
import qualified Data.Text                            as T
import           Development.IDE
import           Development.IDE.Core.PositionMapping
import           Development.IDE.GHC.Compat
import           Development.Shake.Classes
import           GHC.Generics                         (Generic)
import           Ide.PluginUtils                      (mkLspCommand)
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types
import           PrelNames                            (pRELUDE)
import           RnNames                              (findImportUsage,
                                                       getMinimalImports)
import           TcRnMonad                            (initTcWithGbl)
import           TcRnTypes                            (TcGblEnv (tcg_used_gres))

importCommandId :: CommandId
importCommandId = "ImportLensCommand"

-- | The "main" function of a plugin
descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
  (defaultPluginDescriptor plId)
    {
      -- This plugin provides a command handler
      pluginCommands = [importLensCommand],
      -- This plugin defines a new rule
      pluginRules = minimalImportsRule,
      pluginHandlers = mconcat
        [ -- This plugin provides code lenses
          mkPluginHandler STextDocumentCodeLens lensProvider
          -- This plugin provides code actions
        , mkPluginHandler STextDocumentCodeAction codeActionProvider
        ]
    }

-- | The command descriptor
importLensCommand :: PluginCommand IdeState
importLensCommand =
  PluginCommand importCommandId "Explicit import command" runImportCommand

-- | The type of the parameters accepted by our command
data ImportCommandParams = ImportCommandParams WorkspaceEdit
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | The actual command handler
runImportCommand :: CommandFunction IdeState ImportCommandParams
runImportCommand _state (ImportCommandParams edit) = do
  -- This command simply triggers a workspace edit!
  _ <- sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())
  return (Right Null)

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
lensProvider :: PluginMethodHandler IdeState TextDocumentCodeLens
lensProvider
  state -- ghcide state, used to retrieve typechecking artifacts
  pId -- plugin Id
  CodeLensParams {_textDocument = TextDocumentIdentifier {_uri}}
    -- VSCode uses URIs instead of file paths
    -- haskell-lsp provides conversion functions
    | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri _uri = liftIO $
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
codeActionProvider :: PluginMethodHandler IdeState TextDocumentCodeAction
codeActionProvider ideState _pId (CodeActionParams _ _ docId range _context)
  | TextDocumentIdentifier {_uri} <- docId,
    Just nfp <- uriToNormalizedFilePath $ toNormalizedUri _uri = liftIO $
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
              caExplicitImports = InR CodeAction {..}
              _title = "Make all imports explicit"
              _kind = Just CodeActionQuickFix
              _command = Nothing
              _edit = Just WorkspaceEdit {_changes, _documentChanges, _changeAnnotations}
              _changes = Just $ HashMap.singleton _uri $ List edits
              _documentChanges = Nothing
              _diagnostics = Nothing
              _isPreferred = Nothing
              _disabled = Nothing
              _xdata = Nothing
              _changeAnnotations = Nothing
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
  Maybe HscEnvEq ->
  Maybe TcModuleResult ->
  IO ([LImportDecl GhcRn], Maybe [LImportDecl GhcRn])
extractMinimalImports (Just hsc) (Just TcModuleResult {..}) = do
  -- extract the original imports and the typechecking environment
  let tcEnv = tmrTypechecked
      (_, imports, _, _) = tmrRenamed
      ParsedModule {pm_parsed_source = L loc _} = tmrParsed
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
generateLens pId uri importEdit@TextEdit {_range, _newText} = do
  -- The title of the command is just the minimal explicit import decl
  let title = _newText
      -- the code lens has no extra data
      _xdata = Nothing
      -- an edit that replaces the whole declaration with the explicit one
      edit = WorkspaceEdit (Just editsMap) Nothing Nothing
      editsMap = HashMap.fromList [(uri, List [importEdit])]
      -- the command argument is simply the edit
      _arguments = Just [toJSON $ ImportCommandParams edit]
  -- create the command
      _command = Just $ mkLspCommand pId importCommandId title _arguments
  -- create and return the code lens
  return $ Just CodeLens {..}

-- | A helper to run ide actions
runIde :: IdeState -> Action a -> IO a
runIde state = runAction "importLens" state

--------------------------------------------------------------------------------

within :: Range -> SrcSpan -> Bool
within (Range start end) span =
  isInsideSrcSpan start span || isInsideSrcSpan end span

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module Ide.Plugin.RefineImports (descriptor) where

import           Avail                                (AvailInfo (Avail),
                                                       availName, availNames,
                                                       availNamesWithSelectors)
import           Control.Arrow                        (Arrow (second))
import           Control.DeepSeq                      (rwhnf)
import           Control.Monad                        (join)
import           Control.Monad.IO.Class               (liftIO)
import           Data.Aeson.Types
import qualified Data.HashMap.Strict                  as HashMap
import           Data.IORef                           (readIORef)
import           Data.List                            (intercalate)
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (catMaybes, fromMaybe)
import qualified Data.Text                            as T
import           Data.Traversable                     (forM)
import           Development.IDE
import           Development.IDE.Core.PositionMapping
import           Development.IDE.GHC.Compat
import           Development.Shake.Classes
import           GHC.Generics                         (Generic)
import           Ide.Plugin.ExplicitImports           (extractMinimalImports,
                                                       within)
import           Ide.PluginUtils                      (mkLspCommand)
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types
import           PrelNames                            (pRELUDE)
import           RnNames                              (findImportUsage,
                                                       getMinimalImports)
import           TcRnMonad                            (initTcWithGbl,
                                                       tcg_rn_exports,
                                                       tcg_used_gres)

-- | plugin declaration
descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginCommands = [refineImportCommand]
  , pluginRules = refineImportsRule
  , pluginHandlers = mconcat
      [ -- This plugin provides code lenses
        mkPluginHandler STextDocumentCodeLens lensProvider
        -- This plugin provides code actions
      , mkPluginHandler STextDocumentCodeAction codeActionProvider
      ]
  }

refineImportCommandId :: CommandId
refineImportCommandId = "RefineImportLensCommand"

newtype RefineImportCommandParams = RefineImportCommandParams WorkspaceEdit
  deriving Generic
  deriving anyclass (FromJSON, ToJSON)

-- | The command descriptor
refineImportCommand :: PluginCommand IdeState
refineImportCommand =
  PluginCommand
    { commandId = refineImportCommandId
    , commandDesc = "Directly use the imports as oppose to using aggregation module"
    , commandFunc = runRefineImportCommand
    }

-- | The actual command handler
runRefineImportCommand :: CommandFunction IdeState RefineImportCommandParams
runRefineImportCommand _state (RefineImportCommandParams edit) = do
  -- This command simply triggers a workspace edit!
  _ <- sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())
  return (Right Null)

lensProvider :: PluginMethodHandler IdeState TextDocumentCodeLens
lensProvider
  state -- ghcide state
  pId
  CodeLensParams {_textDocument = TextDocumentIdentifier {_uri}}
    -- VSCode uses URIs instead of file paths
    -- haskell-lsp provides conversion functions
    | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri _uri = liftIO $
      do
        mbRefinedImports <-
          runIde state $ useWithStale RefineImports nfp
        case mbRefinedImports of
          -- Implement the provider logic:
          -- for every import, if it's lacking a explicit list, generate a code lens
          Just (RefineImportsResult result, posMapping) -> do
            commands <-
              sequence
                [ generateLens pId _uri edit
                | (imp, Just refinedImports) <- result
                , Just edit <- [mkExplicitEdit posMapping imp refinedImports]
                ]
            return $ Right (List $ catMaybes commands)
          _ -> return $ Right (List [])
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
          mbRefinedImports <- runIde ideState $ use RefineImports nfp
          let edits =
                [ e
                | Just (RefineImportsResult result) <- [mbRefinedImports]
                , (imp, Just refinedImports) <- result
                , Just e <- [mkExplicitEdit zeroMapping imp refinedImports]
                ]
              caExplicitImports = InR CodeAction {..}
              _title = "Refine all imports"
              _kind = Just CodeActionQuickFix
              _command = Nothing
              _edit = Just WorkspaceEdit
                {_changes, _documentChanges, _changeAnnotations}
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

data RefineImports = RefineImports
  deriving (Show, Generic, Eq, Ord)

instance Hashable RefineImports
instance NFData RefineImports
instance Binary RefineImports
type instance RuleResult RefineImports = RefineImportsResult

newtype RefineImportsResult = RefineImportsResult
  {getMinimalImportsResult :: [(LImportDecl GhcRn, Maybe T.Text)]}

instance Show RefineImportsResult where show _ = "<refineImportsResult>"
instance NFData RefineImportsResult where rnf = rwhnf

refineImportsRule :: Rules ()
refineImportsRule = define $ \RefineImports nfp -> do
  -- Get the typechecking artifacts from the module
  tmr <- use TypeCheck nfp
  -- We also need a GHC session with all the dependencies
  hsc <- use GhcSessionDeps nfp

  -- 2 layer map ModuleName -> ModuleName -> [Avails] (exports)
  import2Map <- do
    -- first layer is from current(editing) module to its imports
    ImportMap currIm <- use_ GetImportMap nfp
    forM currIm $ \path -> do
      -- second layer is from the imports of first layer to their imports
      ImportMap importIm <- use_ GetImportMap path
      forM importIm $ \imp_path -> do
        imp_tmr <- use_ TypeCheck imp_path
        return $ tcg_exports $ tmrTypechecked imp_tmr

  -- Use the GHC api to extract the "minimal" imports
  -- We shouldn't blindly refine imports
  -- instead we should generate imports statements
  -- for modules/symbols actually got used
  (imports, mbMinImports) <- liftIO $ extractMinimalImports hsc tmr

  let filterByImport
        :: LImportDecl GhcRn
        -> Map.Map ModuleName [AvailInfo]
        -> Map.Map ModuleName [AvailInfo]
      filterByImport (L _ ImportDecl{ideclHiding = Just (_, L _ names)}) avails =
        let importedNames = map (prettyPrint . ieName . unLoc) names
        in flip Map.filter avails $ \a ->
              any ((`elem` importedNames) . prettyPrint)
                $ concatMap availNamesWithSelectors a
      filterByImport _ _ = mempty
  let constructImport
        :: LImportDecl GhcRn
        -> (ModuleName, [AvailInfo])
        -> LImportDecl GhcRn
      constructImport
        i@(L lim id@ImportDecl
                  {ideclName = L _ mn, ideclHiding = Just (hiding, L _ names)})
        (newModuleName, avails) = L lim id
          { ideclName = noLoc newModuleName
          , ideclHiding = Just (hiding, noLoc newNames)
          }
          where newNames = filter (\n -> any (n `containsAvail`) avails) names
      constructImport lim _ = lim
  let res =
        [ (i, Just
                . T.intercalate "\n"
                . map (T.pack . prettyPrint . constructImport i)
                . Map.toList
                $ filteredInnerImports)
        -- for every minimal imports
        | Just minImports <- [mbMinImports]
        , i@(L _ ImportDecl{ideclName = L _ mn}) <- minImports
        -- we check for the inner imports
        , Just innerImports <- [Map.lookup mn import2Map]
        -- and only get those symbols used
        , filteredInnerImports <- [filterByImport i innerImports]
        -- if no symbols from this modules then don't need to generate new import
        , not $ null filteredInnerImports
        ]
  return ([], RefineImportsResult res <$ mbMinImports)

  where
    -- Check if a name is exposed by AvailInfo (the available information of a module)
    containsAvail :: LIE GhcRn -> AvailInfo -> Bool
    containsAvail name avail =
      any (\an -> prettyPrint an == (prettyPrint . ieName . unLoc $ name))
        $ availNamesWithSelectors avail

--------------------------------------------------------------------------------

mkExplicitEdit :: PositionMapping -> LImportDecl pass -> T.Text -> Maybe TextEdit
mkExplicitEdit posMapping (L src imp) explicit
  | RealSrcSpan l <- src,
    L _ mn <- ideclName imp,
    -- (almost) no one wants to see an refine import list for Prelude
    mn /= moduleName pRELUDE,
    Just rng <- toCurrentRange posMapping $ realSrcSpanToRange l =
    Just $ TextEdit rng explicit
  | otherwise =
    Nothing

-- | Given an import declaration, generate a code lens unless it has an
-- explicit import list or it's qualified
generateLens :: PluginId -> Uri -> TextEdit -> IO (Maybe CodeLens)
generateLens pId uri edits@TextEdit {_range, _newText} = do
  -- The title of the command is just the minimal explicit import decl
  let title = T.intercalate ", " (T.lines _newText)
      -- the code lens has no extra data
      _xdata = Nothing
      -- an edit that replaces the whole declaration with the explicit one
      edit = WorkspaceEdit (Just editsMap) Nothing Nothing
      editsMap = HashMap.fromList [(uri, List [edits])]
      -- the command argument is simply the edit
      _arguments = Just [toJSON $ RefineImportCommandParams edit]
      -- create the command
      _command = Just $ mkLspCommand pId refineImportCommandId title _arguments
  -- create and return the code lens
  return $ Just CodeLens {..}

--------------------------------------------------------------------------------

-- | A helper to run ide actions
runIde :: IdeState -> Action a -> IO a
runIde = runAction "RefineImports"

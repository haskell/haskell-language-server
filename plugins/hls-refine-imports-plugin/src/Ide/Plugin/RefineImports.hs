{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Ide.Plugin.RefineImports (descriptor) where

import Development.IDE
import Ide.Types
import Language.Haskell.LSP.Types
import Development.Shake.Classes
import GHC.Generics (Generic)
import Development.IDE.GHC.Compat
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Maybe (fromMaybe, catMaybes)
import Control.Monad.IO.Class (liftIO)
import Control.DeepSeq (rwhnf)
import TcRnMonad (tcg_used_gres, initTcWithGbl, tcg_rn_exports)
import RnNames (getMinimalImports, findImportUsage)
import Data.IORef (readIORef)
import Development.IDE.Core.PositionMapping (toCurrentRange, PositionMapping)
import PrelNames (pRELUDE)
import Ide.PluginUtils (mkLspCommand)
import Control.Monad (join)
import Data.List (intercalate)
import Data.Traversable (forM)
import Avail (availNamesWithSelectors, availNames, availName, AvailInfo(Avail))
import Control.Arrow (Arrow(second))
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HashMap

-- | plugin declaration
descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginCodeLensProvider = Just lensProvider
  , pluginCommands = [refineImportCommand]
  , pluginRules = refineImportsRule
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
runRefineImportCommand _lspFuncs _state (RefineImportCommandParams edit) = do
  -- This command simply triggers a workspace edit!
  return (Right Null, Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams edit))

lensProvider :: CodeLensProvider IdeState
lensProvider
  _lspFuncs
  state -- ghcide state
  pId -- plugin Id
  CodeLensParams {_textDocument = TextDocumentIdentifier {_uri}}
    -- VSCode uses URIs instead of file paths
    -- haskell-lsp provides conversion functions
    | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri _uri =
      do
        mbRefinedImports <- runAction "RefineImports" state $ useWithStale RefineImports nfp
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
  -- TODO make this parallelized better by using `uses`
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
  -- instead we should generate imports statements for modules/symbols actually got used
  (imports, mbMinImports) <- liftIO $ extractMinimalImports hsc tmr

  let filterByImport :: LImportDecl GhcRn -> Map.Map ModuleName [AvailInfo] -> Map.Map ModuleName [AvailInfo]
      filterByImport (L _ ImportDecl{ideclHiding = Just (_, L _ names)}) avails =
        let importedNames = map (prettyPrint . ieName . unLoc) names
        in flip Map.filter avails $ \a ->
              any ((`elem` importedNames) . prettyPrint) $ concatMap availNamesWithSelectors a
      filterByImport _ _ = mempty
  let constructImport :: LImportDecl GhcRn -> (ModuleName, [AvailInfo]) -> LImportDecl GhcRn
      constructImport 
        i@(L lim id@ImportDecl{ideclName = L _ mn, ideclHiding = Just (hiding, L _ names)}) 
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
      edit = WorkspaceEdit (Just editsMap) Nothing
      editsMap = HashMap.fromList [(uri, List [edits])]
      -- the command argument is simply the edit
      _arguments = Just [toJSON $ RefineImportCommandParams edit]
  -- create the command
  _command <- Just <$> mkLspCommand pId refineImportCommandId title _arguments
  -- create and return the code lens
  return $ Just CodeLens {..}

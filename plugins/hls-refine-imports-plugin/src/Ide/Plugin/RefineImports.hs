{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE CPP                   #-}

module Ide.Plugin.RefineImports (descriptor, Log(..)) where

import           Control.Arrow                        (Arrow (second))
import           Control.DeepSeq                      (rwhnf)
import           Control.Monad                        (join)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Maybe            (MaybeT (MaybeT),
                                                       runMaybeT)
import           Data.Aeson.Types                     hiding (Null)
import           Data.IORef                           (readIORef)
import           Data.List                            (intercalate)
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (catMaybes, fromMaybe)
import qualified Data.Set                             as S
import qualified Data.Text                            as T
import           Data.Traversable                     (forM)
import           Development.IDE
import           Development.IDE.Core.PositionMapping
import           Development.IDE.GHC.Compat
                                                      {- (AvailInfo,
                                                       GenLocated (L), GhcRn,
                                                       HsModule (hsmodImports),
                                                       ImportDecl (ImportDecl, ideclHiding, ideclName),
                                                       LIE, LImportDecl,
                                                       Module (moduleName),
                                                       ModuleName,
                                                       ParsedModule (ParsedModule, pm_parsed_source),
                                                       SrcSpan(..),
                                                       RealSrcSpan(..),
                                                       getLoc, ieName, noLoc,
                                                       tcg_exports, unLoc) -}
import qualified Development.IDE.Core.Shake           as Shake
import           Development.IDE.Graph.Classes
import qualified Development.IDE.Types.Logger         as Logger
import           GHC.Generics                         (Generic)
import           Ide.Plugin.ExplicitImports           (extractMinimalImports,
                                                       within)
import           Ide.PluginUtils                      (mkLspCommand)
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Protocol.Types                   (ApplyWorkspaceEditParams (ApplyWorkspaceEditParams),
                                                       CodeAction (CodeAction, _command, _diagnostics, _disabled, _edit, _isPreferred, _kind, _title, _data_),
                                                       CodeActionKind (CodeActionKind_Custom),
                                                       CodeActionParams (CodeActionParams),
                                                       CodeLens (..),
                                                       CodeLensParams (CodeLensParams, _textDocument),
                                                       TextDocumentIdentifier (TextDocumentIdentifier, _uri),
                                                       TextEdit (..),
                                                       WorkspaceEdit (..),
                                                       type (|?) (InL, InR),
                                                       uriToNormalizedFilePath, Null (Null))
import           Language.LSP.Protocol.Message         (Method (Method_TextDocumentCodeAction, Method_TextDocumentCodeLens),
                                                       SMethod (SMethod_TextDocumentCodeAction, SMethod_TextDocumentCodeLens, SMethod_WorkspaceApplyEdit),)
newtype Log = LogShake Shake.Log deriving Show

instance Pretty Log where
  pretty = \case
    LogShake log -> pretty log

-- | plugin declaration
descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId)
  { pluginCommands = [refineImportCommand]
  , pluginRules = refineImportsRule recorder
  , pluginHandlers = mconcat
      [ -- This plugin provides code lenses
        mkPluginHandler SMethod_TextDocumentCodeLens lensProvider
        -- This plugin provides code actions
      , mkPluginHandler SMethod_TextDocumentCodeAction codeActionProvider
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
  _ <- sendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())
  return (Right $ InR Null)

lensProvider :: PluginMethodHandler IdeState Method_TextDocumentCodeLens
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
          -- for every refined import, generate a code lens
          Just (RefineImportsResult result, posMapping) -> do
            commands <-
              sequence
                [ generateLens pId _uri edit
                | (imp, Just refinedImports) <- result
                , Just edit <- [mkExplicitEdit posMapping imp refinedImports]
                ]
            return $ Right (InL $ catMaybes commands)
          _ -> return $ Right (InL [])
    | otherwise =
      return $ Right (InL [])

-- | Provide one code action to refine all imports
codeActionProvider :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
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
        then return (Right (InL []))
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
              _kind = Just $ CodeActionKind_Custom "quickfix.import.refine"
              _command = Nothing
              _edit = Just WorkspaceEdit
                {_changes, _documentChanges, _changeAnnotations}
              _changes = Just $ Map.singleton _uri edits
              _documentChanges = Nothing
              _diagnostics = Nothing
              _isPreferred = Nothing
              _disabled = Nothing
              _data_ = Nothing
              _changeAnnotations = Nothing
          return $ Right $ InL [caExplicitImports | not (null edits)]
  | otherwise =
    return $ Right $ InL []

--------------------------------------------------------------------------------

data RefineImports = RefineImports
  deriving (Show, Generic, Eq, Ord)

instance Hashable RefineImports
instance NFData RefineImports
type instance RuleResult RefineImports = RefineImportsResult

newtype RefineImportsResult = RefineImportsResult
  {getMinimalImportsResult :: [(LImportDecl GhcRn, Maybe T.Text)]}

instance Show RefineImportsResult where show _ = "<refineImportsResult>"
instance NFData RefineImportsResult where rnf = rwhnf

refineImportsRule :: Recorder (WithPriority Log) -> Rules ()
refineImportsRule recorder = defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \RefineImports nfp -> runMaybeT $ do
  -- Get the typechecking artifacts from the module
  tmr <- MaybeT $ use TypeCheck nfp
  -- We also need a GHC session with all the dependencies
  hsc <- MaybeT $ use GhcSessionDeps nfp

  -- 2 layer map ModuleName -> ModuleName -> [Avails] (exports)
  import2Map <- do
    -- first layer is from current(editing) module to its imports
    ImportMap currIm <- lift $ use_ GetImportMap nfp
    forM currIm $ \path -> do
      -- second layer is from the imports of first layer to their imports
      ImportMap importIm <- lift $ use_ GetImportMap path
      forM importIm $ \imp_path -> do
        imp_hir <- lift $ use_ GetModIface imp_path
        return $ mi_exports $ hirModIface imp_hir

  -- Use the GHC api to extract the "minimal" imports
  -- We shouldn't blindly refine imports
  -- instead we should generate imports statements
  -- for modules/symbols actually got used
  (imports, mbMinImports) <- MaybeT $ liftIO $ extractMinimalImports hsc tmr

  let filterByImport
        :: LImportDecl GhcRn
        -> Map.Map ModuleName [AvailInfo]
        -> Maybe (Map.Map ModuleName [AvailInfo])
#if MIN_VERSION_ghc(9,5,0)
      filterByImport (L _ ImportDecl{ideclImportList = Just (_, L _ names)}) avails =
#else
      filterByImport (L _ ImportDecl{ideclHiding = Just (_, L _ names)}) avails =
#endif
        let importedNames = S.fromList $ map (ieName . unLoc) names
            res = flip Map.filter avails $ \a ->
                    any (`S.member` importedNames)
                      $ concatMap availNamesWithSelectors a
            allFilteredAvailsNames = S.fromList
              $ concatMap availNamesWithSelectors
              $ mconcat
              $ Map.elems res
            -- if there is a function defined in the current module and is used
            -- i.e. if a function is not reexported but defined in current
            -- module then this import cannot be refined
        in if importedNames `S.isSubsetOf` allFilteredAvailsNames
              then Just res
              else Nothing
      filterByImport _ _ = Nothing
  let constructImport
        :: LImportDecl GhcRn
        -> (ModuleName, [AvailInfo])
        -> LImportDecl GhcRn
      constructImport
        i@(L lim id@ImportDecl
#if MIN_VERSION_ghc(9,5,0)
                  {ideclName = L _ mn, ideclImportList = Just (hiding, L _ names)})
#else
                  {ideclName = L _ mn, ideclHiding = Just (hiding, L _ names)})
#endif
        (newModuleName, avails) = L lim id
          { ideclName = noLocA newModuleName
#if MIN_VERSION_ghc(9,5,0)
          , ideclImportList = Just (hiding, noLocA newNames)
#else
          , ideclHiding = Just (hiding, noLocA newNames)
#endif
          }
          where newNames = filter (\n -> any (n `containsAvail`) avails) names
      constructImport lim _ = lim
  let res =
        [ (i, Just
                . T.intercalate "\n"
                . map (printOutputable . constructImport i)
                . Map.toList
                $ filteredInnerImports)
        -- for every minimal imports
        | minImports <- [mbMinImports]
        , i@(L _ ImportDecl{ideclName = L _ mn}) <- minImports
        -- we check for the inner imports
        , Just innerImports <- [Map.lookup mn import2Map]
        -- and only get those symbols used
        , Just filteredInnerImports <- [filterByImport i innerImports]
        -- if no symbols from this modules then don't need to generate new import
        , not $ null filteredInnerImports
        ]
  pure $ RefineImportsResult res

  where
    -- Check if a name is exposed by AvailInfo (the available information of a module)
    containsAvail :: LIE GhcRn -> AvailInfo -> Bool
    containsAvail name avail =
      any (\an -> printOutputable an == (printOutputable . ieName . unLoc $ name))
        $ availNamesWithSelectors avail

--------------------------------------------------------------------------------

mkExplicitEdit :: PositionMapping -> LImportDecl GhcRn -> T.Text -> Maybe TextEdit
mkExplicitEdit posMapping (L src imp) explicit
  | RealSrcSpan l _ <- locA src,
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
  let title = "Refine imports to " <> T.intercalate ", " (T.lines _newText)
      -- the code lens has no extra data
      _data_ = Nothing
      -- an edit that replaces the whole declaration with the explicit one
      edit = WorkspaceEdit (Just editsMap) Nothing Nothing
      editsMap = Map.fromList [(uri, [edits])]
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

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}

module Ide.Plugin.ExplicitImports
  ( descriptor
  , descriptorForModules
  , extractMinimalImports
  , within
  , abbreviateImportTitle
  , Log(..)
  ) where

import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Data.Aeson                           (ToJSON (toJSON),
                                                       Value (Null))
import           Data.Aeson.Types                     (FromJSON)
import qualified Data.HashMap.Strict                  as HashMap
import           Data.IORef                           (readIORef)
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (catMaybes, fromMaybe,
                                                       isJust)
import           Data.String                          (fromString)
import qualified Data.Text                            as T
import           Development.IDE                      hiding (pluginHandlers,
                                                       pluginRules)
import           Development.IDE.Core.PositionMapping
import qualified Development.IDE.Core.Shake           as Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.Graph.Classes
import           Development.IDE.Types.Logger         as Logger (Pretty (pretty))
import           GHC.Generics                         (Generic)
import           Ide.PluginUtils                      (mkLspCommand)
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types

importCommandId :: CommandId
importCommandId = "ImportLensCommand"

newtype Log
  = LogShake Shake.Log
  deriving Show

instance Pretty Log where
  pretty = \case
    LogShake log -> pretty log

-- | The "main" function of a plugin
descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder =
    -- (almost) no one wants to see an explicit import list for Prelude
    descriptorForModules recorder (/= moduleName pRELUDE)

descriptorForModules
    :: Recorder (WithPriority Log)
    -> (ModuleName -> Bool)
      -- ^ Predicate to select modules that will be annotated
    -> PluginId
    -> PluginDescriptor IdeState
descriptorForModules recorder pred plId =
  (defaultPluginDescriptor plId)
    {
      -- This plugin provides a command handler
      pluginCommands = [importLensCommand],
      -- This plugin defines a new rule
      pluginRules = minimalImportsRule recorder,
      pluginHandlers = mconcat
        [ -- This plugin provides code lenses
          mkPluginHandler STextDocumentCodeLens $ lensProvider pred
          -- This plugin provides code actions
        , mkPluginHandler STextDocumentCodeAction $ codeActionProvider pred
        ]
    }

-- | The command descriptor
importLensCommand :: PluginCommand IdeState
importLensCommand =
  PluginCommand importCommandId "Explicit import command" runImportCommand

-- | The type of the parameters accepted by our command
newtype ImportCommandParams = ImportCommandParams WorkspaceEdit
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
lensProvider :: (ModuleName -> Bool) -> PluginMethodHandler IdeState TextDocumentCodeLens
lensProvider
  pred
  state -- ghcide state, used to retrieve typechecking artifacts
  pId -- plugin Id
  CodeLensParams {_textDocument = TextDocumentIdentifier {_uri}}
    -- VSCode uses URIs instead of file paths
    -- haskell-lsp provides conversion functions
    | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri _uri = liftIO $
      do
        mbMinImports <- runAction "MinimalImports" state $ useWithStale MinimalImports nfp
        case mbMinImports of
          -- Implement the provider logic:
          -- for every import, if it's lacking a explicit list, generate a code lens
          Just (MinimalImportsResult minImports, posMapping) -> do
            commands <-
              sequence
                [ generateLens pId _uri edit
                  | (imp, Just minImport) <- minImports,
                    Just edit <- [mkExplicitEdit pred posMapping imp minImport]
                ]
            return $ Right (List $ catMaybes commands)
          _ ->
            return $ Right (List [])
    | otherwise =
      return $ Right (List [])

-- | If there are any implicit imports, provide one code action to turn them all
--   into explicit imports.
codeActionProvider :: (ModuleName -> Bool) -> PluginMethodHandler IdeState TextDocumentCodeAction
codeActionProvider pred ideState _pId (CodeActionParams _ _ docId range _context)
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
                    Just e <- [mkExplicitEdit pred zeroMapping imp explicit]
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

type instance RuleResult MinimalImports = MinimalImportsResult

newtype MinimalImportsResult = MinimalImportsResult
  {getMinimalImportsResult :: [(LImportDecl GhcRn, Maybe T.Text)]}

instance Show MinimalImportsResult where show _ = "<minimalImportsResult>"

instance NFData MinimalImportsResult where rnf = rwhnf

exportedModuleStrings :: ParsedModule -> [String]
exportedModuleStrings ParsedModule{pm_parsed_source = L _ HsModule{..}}
  | Just export <- hsmodExports,
    exports <- unLoc export
    = map (T.unpack . printOutputable) exports
exportedModuleStrings _ = []

minimalImportsRule :: Recorder (WithPriority Log) -> Rules ()
minimalImportsRule recorder = define (cmapWithPrio LogShake recorder) $ \MinimalImports nfp -> do
  -- Get the typechecking artifacts from the module
  tmr <- use TypeCheck nfp
  -- We also need a GHC session with all the dependencies
  hsc <- use GhcSessionDeps nfp
  -- Use the GHC api to extract the "minimal" imports
  (imports, mbMinImports) <- liftIO $ extractMinimalImports hsc tmr
  let importsMap =
        Map.fromList
          [ (realSrcSpanStart l, printOutputable i)
            | L (locA -> RealSrcSpan l _) i <- fromMaybe [] mbMinImports
            , not (isImplicitPrelude i)
          ]
      res =
        [ (i, Map.lookup (realSrcSpanStart l) importsMap)
          | i <- imports
          , RealSrcSpan l _ <- [getLoc i]
        ]
  return ([], MinimalImportsResult res <$ mbMinImports)
  where
    isImplicitPrelude :: (Outputable a) => a -> Bool
    isImplicitPrelude importDecl =
      T.isPrefixOf implicitPreludeImportPrefix (printOutputable importDecl)

-- | This is the prefix of an implicit prelude import which should be ignored,
-- when considering the minimal imports rule
implicitPreludeImportPrefix :: T.Text
implicitPreludeImportPrefix = "import (implicit) Prelude"

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
      emss = exportedModuleStrings tmrParsed
      span = fromMaybe (error "expected real") $ realSpan loc
  -- Don't make suggestions for modules which are also exported, the user probably doesn't want this!
  -- See https://github.com/haskell/haskell-language-server/issues/2079
  let notExportedImports = filter (notExported emss) imports

  -- GHC is secretly full of mutable state
  gblElts <- readIORef (tcg_used_gres tcEnv)

  -- call findImportUsage does exactly what we need
  -- GHC is full of treats like this
  let usage = findImportUsage notExportedImports gblElts
  (_, minimalImports) <-
    initTcWithGbl (hscEnv hsc) tcEnv span $ getMinimalImports usage

  -- return both the original imports and the computed minimal ones
  return (imports, minimalImports)
  where
      notExported :: [String] -> LImportDecl GhcRn -> Bool
      notExported []  _ = True
      notExported exports (L _ ImportDecl{ideclName = L _ name}) =
          not $ any (\e -> ("module " ++ moduleNameString name) == e) exports
extractMinimalImports _ _ = return ([], Nothing)

mkExplicitEdit :: (ModuleName -> Bool) -> PositionMapping -> LImportDecl GhcRn -> T.Text -> Maybe TextEdit
mkExplicitEdit pred posMapping (L (locA -> src) imp) explicit
  -- Explicit import list case
  | ImportDecl {ideclHiding = Just (False, _)} <- imp =
    Nothing
  | not (isQualifiedImport imp),
    RealSrcSpan l _ <- src,
    L _ mn <- ideclName imp,
    pred mn,
    Just rng <- toCurrentRange posMapping $ realSrcSpanToRange l =
    Just $ TextEdit rng explicit
  | otherwise =
    Nothing

-- This number is somewhat arbitrarily chosen. Ideally the protocol would tell us these things,
-- but at the moment I don't believe we know it.
-- 80 columns is traditional, but Haskellers tend to use longer lines (citation needed) and it's
-- probably not too bad if the lens is a *bit* longer than normal lines.
maxColumns :: Int
maxColumns = 120

-- | Given an import declaration, generate a code lens unless it has an
-- explicit import list or it's qualified
generateLens :: PluginId -> Uri -> TextEdit -> IO (Maybe CodeLens)
generateLens pId uri importEdit@TextEdit {_range, _newText} = do
  let
      title = abbreviateImportTitle _newText
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

-- | The title of the command is ideally the minimal explicit import decl, but
-- we don't want to create a really massive code lens (and the decl can be extremely large!).
-- So we abbreviate it to fit a max column size, and indicate how many more items are in the list
-- after the abbreviation
abbreviateImportTitle :: T.Text -> T.Text
abbreviateImportTitle input =
  let
      -- For starters, we only want one line in the title
      oneLineText = T.unwords $ T.lines input
      -- Now, split at the max columns, leaving space for the summary text we're going to add
      -- (conservatively assuming we won't need to print a number larger than 100)
      (prefix, suffix) = T.splitAt (maxColumns - T.length (summaryText 100)) oneLineText
      -- We also want to truncate the last item so we get a "clean" break, rather than half way through
      -- something. The conditional here is just because 'breakOnEnd' doesn't give us quite the right thing
      -- if there are actually no commas.
      (actualPrefix, extraSuffix) = if T.count "," prefix > 0 then T.breakOnEnd "," prefix else (prefix, "")
      actualSuffix = extraSuffix <> suffix

      -- The number of additional items is the number of commas+1
      numAdditionalItems = T.count "," actualSuffix + 1
      -- We want to make text like this: import Foo (AImport, BImport, ... (30 items))
      -- We also want it to look sensible if we end up splitting in the module name itself,
      summaryText n = " ... (" <> fromString (show n) <> " items)"
      -- so we only add a trailing paren if we've split in the export list
      suffixText = summaryText numAdditionalItems <> if T.count "(" prefix > 0 then ")" else ""
      title =
          -- If the original text fits, just use it
          if T.length oneLineText <= maxColumns
          then oneLineText
          else actualPrefix <> suffixText
  in title

--------------------------------------------------------------------------------

-- | A helper to run ide actions
runIde :: IdeState -> Action a -> IO a
runIde = runAction "importLens"

within :: Range -> SrcSpan -> Bool
within (Range start end) span =
  isInsideSrcSpan start span || isInsideSrcSpan end span

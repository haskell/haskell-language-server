{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}
module Ide.Plugin.ExplicitImports
  ( descriptor
  , descriptorForModules
  , abbreviateImportTitle
  , Log(..)
  ) where

import           Control.DeepSeq
import           Control.Lens                         ((&), (?~))
import           Control.Monad.Error.Class            (MonadError (throwError))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Except           (ExceptT)
import           Control.Monad.Trans.Maybe
import qualified Data.Aeson                           as A (ToJSON (toJSON))
import           Data.Aeson.Types                     (FromJSON)
import qualified Data.IntMap                          as IM (IntMap, elems,
                                                             fromList, (!?))
import           Data.IORef                           (readIORef)
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (isNothing, mapMaybe)
import qualified Data.Set                             as S
import           Data.String                          (fromString)
import qualified Data.Text                            as T
import           Data.Traversable                     (for)
import qualified Data.Unique                          as U (hashUnique,
                                                            newUnique)
import           Development.IDE                      hiding (pluginHandlers,
                                                       pluginRules)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping
import qualified Development.IDE.Core.Shake           as Shake
import           Development.IDE.GHC.Compat           hiding ((<+>))
import           Development.IDE.Graph.Classes
import           GHC.Generics                         (Generic)
import           Ide.Plugin.Error                     (PluginError (..),
                                                       getNormalizedFilePathE,
                                                       handleMaybe)
import           Ide.Plugin.RangeMap                  (filterByRange)
import qualified Ide.Plugin.RangeMap                  as RM (RangeMap, fromList)
import           Ide.Plugin.Resolve
import           Ide.PluginUtils
import           Ide.Types
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server

-- This plugin is named explicit-imports for historical reasons. Besides
-- providing code actions and lenses to make imports explicit it also provides
-- code actions and lens to refine imports.

importCommandId :: CommandId
importCommandId = "ImportLensCommand"

data Log
  = LogShake Shake.Log
  | LogWAEResponseError ResponseError
  | forall a. (Pretty a) => LogResolve a


instance Pretty Log where
  pretty = \case
    LogShake logMsg -> pretty logMsg
    LogWAEResponseError rspErr -> "RequestWorkspaceApplyEdit Failed with " <+> pretty rspErr
    LogResolve msg -> pretty msg

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
descriptorForModules recorder modFilter plId =
  let resolveRecorder = cmapWithPrio LogResolve recorder
      codeActionHandlers = mkCodeActionHandlerWithResolve resolveRecorder (codeActionProvider recorder) (codeActionResolveProvider recorder)
  in (defaultPluginDescriptor plId "Provides a code action to make imports explicit")
    {
      -- This plugin provides a command handler
      pluginCommands = [PluginCommand importCommandId "Explicit import command" (runImportCommand recorder)],
      -- This plugin defines a new rule
      pluginRules = minimalImportsRule recorder modFilter,
      pluginHandlers =
         -- This plugin provides code lenses
           mkPluginHandler SMethod_TextDocumentCodeLens (lensProvider recorder)
        <> mkResolveHandler SMethod_CodeLensResolve (lensResolveProvider recorder)
          -- This plugin provides code actions
        <> codeActionHandlers

    }

-- | The actual command handler
runImportCommand :: Recorder (WithPriority Log) -> CommandFunction IdeState IAResolveData
runImportCommand recorder ideState _ eird@(ResolveOne _ _) = do
  wedit <- resolveWTextEdit ideState eird
  _ <- lift $ sendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wedit) logErrors
  return $ InR  Null
  where logErrors (Left re@(ResponseError{})) = do
          logWith recorder Error (LogWAEResponseError re)
          pure ()
        logErrors (Right _) = pure ()
runImportCommand _ _ _ rd = do
  throwError $ PluginInvalidParams (T.pack $ "Unexpected argument for command handler:" <> show rd)


-- | We provide two code lenses for imports. The first lens makes imports
-- explicit. For example, for the module below:
-- > import Data.List
-- > f = intercalate " " . sortBy length
-- the provider should produce one code lens associated to the import statement:
-- > import Data.List (intercalate, sortBy)
--
-- The second one allows us to import functions directly from the original
-- module. For example, for the following import
-- > import Random.ReExporting.Module (liftIO)
-- the provider should produce one code lens associated to the import statement:
-- > Refine imports to import Control.Monad.IO.Class (liftIO)
lensProvider :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'Method_TextDocumentCodeLens
lensProvider _  state _ CodeLensParams {_textDocument = TextDocumentIdentifier {_uri}} = do
    nfp <- getNormalizedFilePathE _uri
    (ImportActionsResult{forLens}, pm) <- runActionE "ImportActions" state $ useWithStaleE ImportActions nfp
    let lens = [ generateLens _uri newRange int
                | (range, int) <- forLens
                , Just newRange <- [toCurrentRange pm range]]
    pure $ InL lens
  where -- because these are non resolved lenses we only need the range and a
        -- unique id to later resolve them with. These are for both refine
        -- import lenses and for explicit import lenses.
        generateLens :: Uri  -> Range -> Int -> CodeLens
        generateLens uri range int =
          CodeLens { _data_ = Just $ A.toJSON $ ResolveOne uri int
                   , _range = range
                   , _command = Nothing }

lensResolveProvider :: Recorder (WithPriority Log) -> ResolveFunction IdeState IAResolveData 'Method_CodeLensResolve
lensResolveProvider _ ideState plId cl uri rd@(ResolveOne _ uid) = do
    nfp <- getNormalizedFilePathE uri
    (ImportActionsResult{forResolve}, _) <- runActionE "ImportActions" ideState $ useWithStaleE ImportActions nfp
    target <- handleMaybe PluginStaleResolve $ forResolve IM.!? uid
    let updatedCodeLens = cl & L.command ?~  mkCommand plId target
    pure updatedCodeLens
  where mkCommand ::  PluginId -> ImportEdit -> Command
        mkCommand pId (ImportEdit{ieResType, ieText}) =
          let -- The only new thing we need to provide to resolve a lens is the
              -- title, as the unique Id is the same to resolve the lens title
              -- as it is to apply the lens through a command.
              -- The title is written differently depending on what type of lens
              -- it is.
              title ExplicitImport = abbreviateImportTitle ieText
              title RefineImport = "Refine imports to " <> T.intercalate ", " (T.lines ieText)
          in mkLspCommand pId importCommandId (title ieResType) (Just [A.toJSON rd])
lensResolveProvider _ _ _ _ _ rd = do
   throwError $ PluginInvalidParams (T.pack $ "Unexpected argument for lens resolve handler: " <> show rd)

-- |For explicit imports: If there are any implicit imports, provide both one
-- code action per import to make that specific import explicit, and one code
-- action to turn them all into explicit imports. For refine imports: If there
-- are any reexported imports, provide both one code action per import to refine
-- that specific import, and one code action to refine all imports.
codeActionProvider :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'Method_TextDocumentCodeAction
codeActionProvider _ ideState _pId (CodeActionParams _ _ TextDocumentIdentifier {_uri} range _context) = do
    nfp <- getNormalizedFilePathE _uri
    (ImportActionsResult{forCodeActions}, pm) <- runActionE "ImportActions" ideState $ useWithStaleE ImportActions nfp
    newRange <- toCurrentRangeE pm range
    let relevantCodeActions = filterByRange newRange forCodeActions
        allExplicit =
          [InR $ mkCodeAction "Make all imports explicit" (Just $ A.toJSON $ ExplicitAll _uri)
          -- We should only provide this code action if there are any code
          -- of this type
          | any (\x -> iaResType x == ExplicitImport) relevantCodeActions]
        allRefine =
          [InR $ mkCodeAction "Refine all imports" (Just $ A.toJSON $ RefineAll _uri)
          -- We should only provide this code action if there are any code
          -- of this type
          | any (\x -> iaResType x == RefineImport) relevantCodeActions]
        -- The only thing different in making the two types of code actions, is
        -- the title. The actual resolve data type, ResolveOne is used by both
        -- of them
        toCodeAction uri (ImportAction _ int ExplicitImport) =
          mkCodeAction "Make this import explicit" (Just $ A.toJSON $ ResolveOne uri int)
        toCodeAction uri (ImportAction _  int RefineImport) =
          mkCodeAction "Refine this import" (Just $ A.toJSON $ ResolveOne uri int)
    pure $ InL ((InR . toCodeAction _uri <$> relevantCodeActions) <> allExplicit <> allRefine)
    where mkCodeAction title data_  =
            CodeAction
            { _title = title
            , _kind = Just CodeActionKind_QuickFix
            , _command = Nothing
            , _edit = Nothing
            , _diagnostics = Nothing
            , _isPreferred = Nothing
            , _disabled = Nothing
            , _data_ = data_}

codeActionResolveProvider :: Recorder (WithPriority Log) -> ResolveFunction IdeState IAResolveData 'Method_CodeActionResolve
codeActionResolveProvider _ ideState _ ca _ rd = do
    wedit <- resolveWTextEdit ideState rd
    pure $ ca & L.edit ?~ wedit
--------------------------------------------------------------------------------

resolveWTextEdit :: IdeState -> IAResolveData -> ExceptT PluginError (LspT Config IO) WorkspaceEdit
-- Providing the edit for the command, or the resolve for the code action is
-- completely generic, as all we need is the unique id and the text edit.
resolveWTextEdit ideState (ResolveOne uri int) = do
  nfp <- getNormalizedFilePathE uri
  (ImportActionsResult{forResolve}, pm) <- runActionE "ImportActions" ideState $ useWithStaleE ImportActions nfp
  iEdit <- handleMaybe PluginStaleResolve $ forResolve IM.!? int
  pure $ mkWorkspaceEdit uri [iEdit] pm
resolveWTextEdit ideState (ExplicitAll uri) = do
  nfp <- getNormalizedFilePathE uri
  (ImportActionsResult{forResolve}, pm) <- runActionE "ImportActions" ideState $ useWithStaleE ImportActions nfp
  let edits = [ ie | ie@ImportEdit{ieResType = ExplicitImport} <- IM.elems forResolve]
  pure $ mkWorkspaceEdit uri edits pm
resolveWTextEdit ideState (RefineAll uri) = do
  nfp <- getNormalizedFilePathE uri
  (ImportActionsResult{forResolve}, pm) <- runActionE "ImportActions" ideState $ useWithStaleE ImportActions nfp
  let edits = [ re | re@ImportEdit{ieResType = RefineImport} <- IM.elems forResolve]
  pure $ mkWorkspaceEdit uri edits pm
mkWorkspaceEdit :: Uri -> [ImportEdit] -> PositionMapping -> WorkspaceEdit
mkWorkspaceEdit uri edits pm =
      WorkspaceEdit {_changes = Just $ Map.singleton uri  (mapMaybe toWEdit edits)
                    , _documentChanges = Nothing
                    , _changeAnnotations = Nothing}
  where toWEdit ImportEdit{ieRange, ieText} =
          let newRange = toCurrentRange pm ieRange
          in (\r -> TextEdit r ieText) <$> newRange

data ImportActions = ImportActions
  deriving (Show, Generic, Eq, Ord)

instance Hashable ImportActions

instance NFData ImportActions

type instance RuleResult ImportActions = ImportActionsResult

data ResultType = ExplicitImport | RefineImport
  deriving Eq

data ImportActionsResult = ImportActionsResult
  { -- |For providing the code lenses we need to have a range, and a unique id
    -- that is later resolved to the new text for each import. It is stored in
    -- a list, because we always need to provide all the code lens in a file.
    forLens        :: [(Range, Int)]
    -- |For the code actions we have the same data as for the code lenses, but
    -- we store it in a RangeMap, because that allows us to filter on a specific
    -- range with better performance, and code actions are almost always only
    -- requested for a specific range
  , forCodeActions :: RM.RangeMap ImportAction
    -- |For resolve we have an intMap where for every previously provided unique id
    -- we provide a textEdit to allow our code actions or code lens to be resolved
  , forResolve     :: IM.IntMap ImportEdit }

-- |For resolving code lenses and code actions we need standard text edit stuff,
-- such as range and text, and then we need the result type, because we use this
-- for code lenses which need to create a appropriate title
data ImportEdit = ImportEdit { ieRange :: Range, ieText :: T.Text, ieResType :: ResultType}

-- |The necessary data for providing code actions: the range, a unique ID for
-- later resolving the action, and the type of action for giving a proper name.
data ImportAction = ImportAction { iaRange :: Range, iaUniqueId :: Int, iaResType :: ResultType}

instance Show ImportActionsResult where show _ = "<ImportActionsResult>"

instance NFData ImportActionsResult where rnf = rwhnf

data IAResolveData = ResolveOne
                      { uri      :: Uri
                      , importId :: Int }
                    | ExplicitAll
                      { uri :: Uri }
                    | RefineAll
                      { uri :: Uri }
                    deriving (Generic, Show, A.ToJSON, FromJSON)

exportedModuleStrings :: ParsedModule -> [String]
exportedModuleStrings ParsedModule{pm_parsed_source = L _ HsModule{..}}
  | Just export <- hsmodExports,
    exports <- unLoc export
    = map (T.unpack . printOutputable) exports
exportedModuleStrings _ = []

minimalImportsRule :: Recorder (WithPriority Log) -> (ModuleName -> Bool) -> Rules ()
minimalImportsRule recorder modFilter = defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \ImportActions nfp -> runMaybeT $ do
  -- Get the typechecking artifacts from the module
  tmr <- MaybeT $ use TypeCheck nfp
  -- We also need a GHC session with all the dependencies
  hsc <- MaybeT $ use GhcSessionDeps nfp

  -- refine imports: 2 layer map ModuleName -> ModuleName -> [Avails] (exports)
  import2Map <- do
    -- first layer is from current(editing) module to its imports
    ImportMap currIm <- MaybeT $ use GetImportMap nfp
    for currIm $ \path -> do
      -- second layer is from the imports of first layer to their imports
      ImportMap importIm <- MaybeT $ use GetImportMap path
      for importIm $ \imp_path -> do
        imp_hir <- MaybeT $ use GetModIface imp_path
        return $ mi_exports $ hirModIface imp_hir

  -- Use the GHC api to extract the "minimal" imports
  locationImportWithMinimal <- MaybeT $ liftIO $ extractMinimalImports hsc tmr

  let minimalImportsResult =
        [ (range, (printOutputable minImport, ExplicitImport))
          | (location, impDecl, minImport) <- locationImportWithMinimal
          , not (isQualifiedImport impDecl)
          , not (isExplicitImport impDecl)
          , let L _ moduleName = ideclName impDecl
          , modFilter moduleName
          , let range = realSrcSpanToRange location]

      refineImportsResult =
        [ (range, (T.intercalate "\n"
                . map (printOutputable . constructImport origImport minImport)
                . Map.toList
                $ filteredInnerImports, RefineImport))
        -- for every minimal imports
        | (location, origImport, minImport@(ImportDecl{ideclName = L _ mn})) <- locationImportWithMinimal
        -- (almost) no one wants to see an refine import list for Prelude
        , mn /= moduleName pRELUDE
        -- we check for the inner imports
        , Just innerImports <- [Map.lookup mn import2Map]
        -- and only get those symbols used
        , Just filteredInnerImports <- [filterByImport minImport innerImports]
        -- if no symbols from this modules then don't need to generate new import
        , not $ null filteredInnerImports
        -- and then convert that to a Range
        , let range = realSrcSpanToRange location
        ]
  uniqueAndRangeAndText <- liftIO $ for (minimalImportsResult ++ refineImportsResult) $ \rt -> do
                                u <- U.hashUnique <$> U.newUnique
                                pure (u,  rt)
  let rangeAndUnique =  [ ImportAction r u rt | (u, (r, (_, rt))) <- uniqueAndRangeAndText ]
  pure ImportActionsResult
                      { forLens = (\ImportAction{..} -> (iaRange, iaUniqueId)) <$> rangeAndUnique
                      , forCodeActions = RM.fromList iaRange rangeAndUnique
                      , forResolve =  IM.fromList ((\(u, (r, (te, ty))) -> (u, ImportEdit r te ty)) <$> uniqueAndRangeAndText) }

--------------------------------------------------------------------------------

-- | Use the ghc api to extract a minimal, explicit set of imports for this module
extractMinimalImports ::
  HscEnvEq ->
  TcModuleResult ->
  IO (Maybe [(RealSrcSpan, ImportDecl GhcRn, ImportDecl GhcRn)])
extractMinimalImports hsc TcModuleResult {..} = runMaybeT $ do
  -- extract the original imports and the typechecking environment
  let tcEnv = tmrTypechecked
#if MIN_VERSION_ghc(9,9,0)
      (_, imports, _, _, _) = tmrRenamed
#else
      (_, imports, _, _) = tmrRenamed
#endif
      ParsedModule {pm_parsed_source = L loc _} = tmrParsed
      emss = exportedModuleStrings tmrParsed
  Just srcSpan <- pure $ realSpan loc
  -- Don't make suggestions for modules which are also exported, the user probably doesn't want this!
  -- See https://github.com/haskell/haskell-language-server/issues/2079
  let notExportedImports = filter (notExported emss) imports

  -- GHC is secretly full of mutable state
  gblElts <- liftIO $ readIORef (tcg_used_gres tcEnv)

  -- call findImportUsage does exactly what we need
  -- GHC is full of treats like this
  let usage = findImportUsage notExportedImports gblElts
  (_, Just minimalImports) <- liftIO $
    initTcWithGbl (hscEnv hsc) tcEnv srcSpan $ getMinimalImports usage

  let minimalImportsMap =
        Map.fromList
          [ (realSrcSpanStart l, impDecl)
            | L (locA -> RealSrcSpan l _) impDecl <- minimalImports
          ]
      results =
          [ (location, imp, minImport)
          | L (locA -> RealSrcSpan location _) imp <- imports
          , Just minImport <- [Map.lookup (realSrcSpanStart location) minimalImportsMap]]
  -- return both the original imports and the computed minimal ones
  return results
  where
      notExported :: [String] -> LImportDecl GhcRn -> Bool
      notExported []  _ = True
      notExported exports (L _ ImportDecl{ideclName = L _ name}) =
          not $ any (\e -> ("module " ++ moduleNameString name) == e) exports

isExplicitImport :: ImportDecl GhcRn -> Bool
#if MIN_VERSION_ghc(9,5,0)
isExplicitImport ImportDecl {ideclImportList = Just (Exactly, _)} = True
#else
isExplicitImport ImportDecl {ideclHiding = Just (False, _)}       = True
#endif
isExplicitImport _                                                = False

-- This number is somewhat arbitrarily chosen. Ideally the protocol would tell us these things,
-- but at the moment I don't believe we know it.
-- 80 columns is traditional, but Haskellers tend to use longer lines (citation needed) and it's
-- probably not too bad if the lens is a *bit* longer than normal lines.
maxColumns :: Int
maxColumns = 120


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
      summaryText :: Int -> T.Text
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


filterByImport :: ImportDecl GhcRn -> Map.Map ModuleName [AvailInfo] -> Maybe (Map.Map ModuleName [AvailInfo])
#if MIN_VERSION_ghc(9,5,0)
filterByImport (ImportDecl{ideclImportList = Just (_, L _ names)})
#else
filterByImport (ImportDecl{ideclHiding = Just (_, L _ names)})
#endif
  avails =
      -- if there is a function defined in the current module and is used
      -- i.e. if a function is not reexported but defined in current
      -- module then this import cannot be refined
  if importedNames `S.isSubsetOf` allFilteredAvailsNames
    then Just res
    else Nothing
  where importedNames = S.fromList $ map (ieName . unLoc) names
        res = flip Map.filter avails $ \a ->
                any (`S.member` importedNames)
                  $ concatMap
                      getAvailNames
                      a
        allFilteredAvailsNames = S.fromList
          $ concatMap getAvailNames
          $ mconcat
          $ Map.elems res
filterByImport _ _ = Nothing

constructImport :: ImportDecl GhcRn -> ImportDecl GhcRn -> (ModuleName, [AvailInfo]) -> ImportDecl GhcRn
#if MIN_VERSION_ghc(9,5,0)
constructImport ImportDecl{ideclQualified = qualified, ideclImportList = origHiding} imd@ImportDecl{ideclImportList = Just (hiding, L _ names)}
#else
constructImport ImportDecl{ideclQualified = qualified, ideclHiding = origHiding} imd@ImportDecl{ideclHiding = Just (hiding, L _ names)}
#endif
  (newModuleName, avails) = imd
    { ideclName = noLocA newModuleName
#if MIN_VERSION_ghc(9,5,0)
    , ideclImportList = if isNothing origHiding && qualified /= NotQualified
                        then Nothing
                        else Just (hiding, noLocA newNames)
#else
    , ideclHiding = if isNothing origHiding && qualified /= NotQualified
                        then Nothing
                        else Just (hiding, noLocA newNames)
#endif
    }
    where newNames = filter (\n -> any (n `containsAvail`) avails) names
          -- Check if a name is exposed by AvailInfo (the available information of a module)
          containsAvail :: LIE GhcRn -> AvailInfo -> Bool
          containsAvail name avail =
            any (\an -> printOutputable an == (printOutputable . ieName . unLoc $ name))
              $ getAvailNames avail

constructImport _ lim _ = lim

getAvailNames :: AvailInfo -> [Name]
getAvailNames =
#if MIN_VERSION_ghc(9,7,0)
  availNames
#else
  availNamesWithSelectors
#endif

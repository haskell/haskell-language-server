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
import           Control.Lens                         ((&), (?~))
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
import           Data.String                          (fromString)
import qualified Data.Text                            as T
import           Data.Traversable                     (for)
import qualified Data.Unique                          as U (hashUnique,
                                                            newUnique)
import           Development.IDE                      hiding (pluginHandlers,
                                                       pluginRules)
import           Development.IDE.Core.PositionMapping
import qualified Development.IDE.Core.Shake           as Shake
import           Development.IDE.GHC.Compat           hiding ((<+>))
import           Development.IDE.Graph.Classes
import           GHC.Generics                         (Generic)
import           Ide.Plugin.RangeMap                  (filterByRange)
import qualified Ide.Plugin.RangeMap                  as RM (RangeMap, fromList)
import           Ide.Plugin.Resolve
import           Ide.PluginUtils                      (getNormalizedFilePath,
                                                       handleMaybe,
                                                       handleMaybeM,
                                                       pluginResponse)
import           Ide.Types
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server

importCommandId :: CommandId
importCommandId = "ImportLensCommand"

data Log
  = LogShake Shake.Log
  | LogWAEResponseError ResponseError
  deriving Show

instance Pretty Log where
  pretty = \case
    LogShake logMsg -> pretty logMsg
    LogWAEResponseError rspErr -> "RequestWorkspaceApplyEdit Failed with " <+> viaShow rspErr

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
  (defaultPluginDescriptor plId)
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
        <> mkCodeActionHandlerWithResolve (codeActionProvider recorder) (codeActionResolveProvider recorder)

    }

-- | The actual command handler
runImportCommand :: Recorder (WithPriority Log) -> CommandFunction IdeState EIResolveData
runImportCommand recorder ideState eird@(ResolveOne _ _) = pluginResponse $ do
  wedit <- resolveWTextEdit ideState eird
  _ <- lift $ sendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wedit) logErrors
  return $ InR  Null
  where logErrors (Left re@(ResponseError{})) = do
          logWith recorder Error (LogWAEResponseError re)
          pure ()
        logErrors (Right _) = pure ()
runImportCommand _ _ (ResolveAll _) = do
  pure $ Left $ ResponseError (InR ErrorCodes_InvalidParams) "Unexpected argument for command handler: ResolveAll" Nothing

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
lensProvider :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'Method_TextDocumentCodeLens
lensProvider _  state _ CodeLensParams {_textDocument = TextDocumentIdentifier {_uri}}
  = pluginResponse $ do
    nfp <- getNormalizedFilePath _uri
    mbMinImports <- liftIO $ runAction "MinimalImports" state $ use MinimalImports nfp
    case mbMinImports of
      Just (MinimalImportsResult{forLens}) -> do
        let lens = [ generateLens _uri range int
                    | (range, int) <- forLens]
        pure $ InL lens
      _ ->
        pure $ InL []
  where generateLens :: Uri  -> Range -> Int -> CodeLens
        generateLens uri range int =
          CodeLens { _data_ = Just $ A.toJSON $ ResolveOne uri int
                   , _range = range
                   , _command = Nothing }

lensResolveProvider :: Recorder (WithPriority Log) -> ResolveFunction IdeState EIResolveData 'Method_CodeLensResolve
lensResolveProvider _ ideState plId cl uri rd@(ResolveOne _ uid)
  = pluginResponse $ do
    nfp <- getNormalizedFilePath uri
    (MinimalImportsResult{forResolve}) <-
      handleMaybeM "Unable to run Minimal Imports"
      $ liftIO
      $ runAction "MinimalImports" ideState $ use MinimalImports nfp
    target <- handleMaybe "Unable to resolve lens" $ forResolve IM.!? uid
    let updatedCodeLens = cl & L.command ?~  mkCommand plId target
    pure updatedCodeLens
  where mkCommand ::  PluginId -> TextEdit -> Command
        mkCommand pId TextEdit{_newText} =
          let title = abbreviateImportTitle _newText
          in mkLspCommand pId importCommandId title (Just $ [A.toJSON rd])
lensResolveProvider _ _ _ _ _ (ResolveAll _) = do
   pure $ Left $ ResponseError (InR ErrorCodes_InvalidParams) "Unexpected argument for lens resolve handler: ResolveAll" Nothing

-- | If there are any implicit imports, provide both one code action per import
--   to make that specific import explicit, and one code action to turn them all
--   into explicit imports.
codeActionProvider :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'Method_TextDocumentCodeAction
codeActionProvider _ ideState _pId (CodeActionParams _ _ TextDocumentIdentifier {_uri} range _context)
  = pluginResponse $ do
    nfp <- getNormalizedFilePath _uri
    (MinimalImportsResult{forCodeActions}) <-
      handleMaybeM "Unable to run Minimal Imports"
        $ liftIO
        $ runAction "MinimalImports" ideState $ use MinimalImports nfp
    let relevantCodeActions = filterByRange range forCodeActions
        allExplicit =
          [InR $ mkCodeAction "Make all imports explicit" (Just $ A.toJSON $ ResolveAll _uri)
          | not $ null relevantCodeActions ]
        toCodeAction uri (_, int) =
          mkCodeAction "Make this import explicit" (Just $ A.toJSON $ ResolveOne uri int)
    pure $ InL ((InR . toCodeAction _uri <$> relevantCodeActions) <> allExplicit)
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

codeActionResolveProvider :: Recorder (WithPriority Log) -> ResolveFunction IdeState EIResolveData 'Method_CodeActionResolve
codeActionResolveProvider _ ideState _ ca _ rd =
  pluginResponse $ do
    wedit <- resolveWTextEdit ideState rd
    pure $ ca & L.edit ?~ wedit
--------------------------------------------------------------------------------

resolveWTextEdit :: IdeState -> EIResolveData -> ExceptT String (LspT Config IO) WorkspaceEdit
resolveWTextEdit ideState (ResolveOne uri int) = do
  nfp <- getNormalizedFilePath uri
  (MinimalImportsResult{forResolve}) <-
    handleMaybeM "Unable to run Minimal Imports"
    $ liftIO
    $ runAction "MinimalImports" ideState $ use MinimalImports nfp
  tedit <- handleMaybe "Unable to resolve text edit" $ forResolve IM.!? int
  pure $ mkWorkspaceEdit uri [tedit]
resolveWTextEdit ideState (ResolveAll uri) = do
  nfp <- getNormalizedFilePath uri
  (MinimalImportsResult{forResolve}) <-
    handleMaybeM "Unable to run Minimal Imports"
    $ liftIO
    $ runAction "MinimalImports" ideState $ use MinimalImports nfp
  let edits = IM.elems forResolve
  pure $ mkWorkspaceEdit uri edits

mkWorkspaceEdit :: Uri -> [TextEdit] -> WorkspaceEdit
mkWorkspaceEdit uri edits =
      WorkspaceEdit {_changes = Just $ Map.fromList [(uri, edits)]
                    , _documentChanges = Nothing
                    , _changeAnnotations = Nothing}

data MinimalImports = MinimalImports
  deriving (Show, Generic, Eq, Ord)

instance Hashable MinimalImports

instance NFData MinimalImports

type instance RuleResult MinimalImports = MinimalImportsResult

data MinimalImportsResult = MinimalImportsResult
  { -- |For providing the code lenses we need to have a range, and a unique id
    -- that is later resolved to the new text for each import. It is stored in
    -- a list, because we always need to provide all the code lens in a file.
    forLens        :: [(Range, Int)]
    -- |For the code actions we have the same data as for the code lenses, but
    -- we store it in a RangeMap, because that allows us to filter on a specific
    -- range with better performance, and code actions are almost always only
    -- requested for a specific range
  , forCodeActions :: RM.RangeMap (Range, Int)
    -- |For resolve we have an intMap where for every previously provided unique id
    -- we provide a textEdit to allow our code actions or code lens to be resolved
  , forResolve     :: IM.IntMap TextEdit }

instance Show MinimalImportsResult where show _ = "<minimalImportsResult>"

instance NFData MinimalImportsResult where rnf = rwhnf

data EIResolveData = ResolveOne
                      { uri      :: Uri
                      , importId :: Int }
                    | ResolveAll
                      { uri :: Uri }
                    deriving (Generic, A.ToJSON, FromJSON)

exportedModuleStrings :: ParsedModule -> [String]
exportedModuleStrings ParsedModule{pm_parsed_source = L _ HsModule{..}}
  | Just export <- hsmodExports,
    exports <- unLoc export
    = map (T.unpack . printOutputable) exports
exportedModuleStrings _ = []

minimalImportsRule :: Recorder (WithPriority Log) -> (ModuleName -> Bool) -> Rules ()
minimalImportsRule recorder modFilter = defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \MinimalImports nfp -> runMaybeT $ do
  -- Get the typechecking artifacts from the module
  (tmr, tmrpm) <- MaybeT $ useWithStale TypeCheck nfp
  -- We also need a GHC session with all the dependencies
  (hsc, _) <- MaybeT $ useWithStale GhcSessionDeps nfp
  -- Use the GHC api to extract the "minimal" imports
  (imports, mbMinImports) <- MaybeT $ liftIO $ extractMinimalImports hsc tmr
  let importsMap =
        Map.fromList
          [ (realSrcSpanStart l, printOutputable i)
            | L (locA -> RealSrcSpan l _) i <- mbMinImports
          ]
      res =
        [ (newRange, minImport)
          | imp@(L _ impDecl) <- imports
          , not (isQualifiedImport impDecl)
          , not (isExplicitImport impDecl)
          , let L _ moduleName = ideclName impDecl
          , modFilter moduleName
          , RealSrcSpan location _ <- [getLoc imp]
          , let range = realSrcSpanToRange location
          , Just minImport <- [Map.lookup (realSrcSpanStart location) importsMap]
          , Just newRange <- [toCurrentRange tmrpm range]
        ]
  uniqueAndRangeAndText <- liftIO $ for res $ \rt -> do
                                u <- U.hashUnique <$> U.newUnique
                                pure (u,  rt)
  let rangeAndUnique =  [ (r, u) | (u, (r, _)) <- uniqueAndRangeAndText ]
  pure MinimalImportsResult
                      { forLens = rangeAndUnique
                      , forCodeActions = RM.fromList fst rangeAndUnique
                      , forResolve =  IM.fromList ((\(i, (r, t)) -> (i, TextEdit r t)) <$> uniqueAndRangeAndText) }

--------------------------------------------------------------------------------

-- | Use the ghc api to extract a minimal, explicit set of imports for this module
extractMinimalImports ::
  HscEnvEq ->
  TcModuleResult ->
  IO (Maybe ([LImportDecl GhcRn], [LImportDecl GhcRn]))
extractMinimalImports hsc TcModuleResult {..} = runMaybeT $ do
  -- extract the original imports and the typechecking environment
  let tcEnv = tmrTypechecked
      (_, imports, _, _) = tmrRenamed
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

  -- return both the original imports and the computed minimal ones
  return (imports, minimalImports)
  where
      notExported :: [String] -> LImportDecl GhcRn -> Bool
      notExported []  _ = True
      notExported exports (L _ ImportDecl{ideclName = L _ name}) =
          not $ any (\e -> ("module " ++ moduleNameString name) == e) exports
#if !MIN_VERSION_ghc (9,0,0)
      notExported _ _ = True
#endif

isExplicitImport :: ImportDecl GhcRn -> Bool
#if MIN_VERSION_ghc (9,5,0)
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

within :: Range -> SrcSpan -> Bool
within (Range start end) srcSpan =
  isInsideSrcSpan start srcSpan || isInsideSrcSpan end srcSpan

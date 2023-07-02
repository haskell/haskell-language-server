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
import           Control.Monad                        (replicateM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Except           (throwE)
import           Data.Aeson                           (Result (Success),
                                                       ToJSON (toJSON),
                                                       Value (Null), fromJSON)
import           Data.Aeson.Types                     (FromJSON)
import qualified Data.HashMap.Strict                  as HashMap
import qualified Data.IntMap                          as IM (IntMap, elems,
                                                             fromList, (!?))
import           Data.IORef                           (readIORef)
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (catMaybes, fromMaybe,
                                                       isJust)
import           Data.String                          (fromString)
import qualified Data.Text                            as T
import qualified Data.Unique                          as U (hashUnique,
                                                            newUnique)
import           Development.IDE                      hiding (pluginHandlers,
                                                       pluginRules)
import           Development.IDE.Core.PositionMapping
import qualified Development.IDE.Core.Shake           as Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.Graph.Classes
import           Development.IDE.Types.Logger         as Logger (Pretty (pretty))
import           GHC.Generics                         (Generic)
import           Ide.Plugin.RangeMap                  (filterByRange)
import qualified Ide.Plugin.RangeMap                  as RM (RangeMap, fromList)
import           Ide.PluginUtils                      (getNormalizedFilePath,
                                                       handleMaybe,
                                                       handleMaybeM,
                                                       mkLspCommand,
                                                       pluginResponse)
import           Ide.Types
import           Language.LSP.Protocol.Lens           (HasDocumentChanges (documentChanges))
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types          hiding (Null)
import           Language.LSP.Server

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
      pluginCommands = [PluginCommand importCommandId "Explicit import command" runImportCommand],
      -- This plugin defines a new rule
      pluginRules = minimalImportsRule recorder pred,
      pluginHandlers =
         -- This plugin provides code lenses
           mkPluginHandler SMethod_TextDocumentCodeLens (lensProvider pred)
        <> mkPluginHandler SMethod_CodeLensResolve lensResolveProvider
          -- This plugin provides code actions
        <> mkCodeActionHandlerWithResolve (codeActionProvider pred) codeActionResolveProvider

    }

-- | The actual command handler
runImportCommand :: CommandFunction IdeState EIResolveData
runImportCommand ideState eird = pluginResponse $ do
  case eird of
    ResolveOne uri int -> do
      nfp <- getNormalizedFilePath uri
      (MinimalImportsResult _ _ resolveMinImp) <-
        handleMaybeM "Unable to run Minimal Imports"
        $ liftIO
        $ runAction "MinimalImports" ideState $ use MinimalImports nfp
      (range, text) <- handleMaybe "Unable to resolve lens" $ resolveMinImp IM.!? int
      let edit = mkWorkspaceEdit uri range text
      _ <- lift $ sendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())
      return Null
  where mkWorkspaceEdit uri range text=
          WorkspaceEdit {_changes = Just $ Map.fromList [(uri, [TextEdit range text])]
                        , _documentChanges = Nothing
                        , _changeAnnotations = Nothing}

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
lensProvider :: (ModuleName -> Bool) -> PluginMethodHandler IdeState Method_TextDocumentCodeLens
lensProvider
  pred
  state -- ghcide state, used to retrieve typechecking artifacts
  pId -- plugin Id
  CodeLensParams {_textDocument = TextDocumentIdentifier {_uri}}
    -- VSCode uses URIs instead of file paths
    -- haskell-lsp provides conversion functions
    | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri _uri =
      do
        mbMinImports <- liftIO $ runAction "MinimalImports" state $ useWithStale MinimalImports nfp
        case mbMinImports of
          Just (MinimalImportsResult minImports _ _, posMapping) -> do
            let lens = [ generateLens _uri curRange int
                       | (range, int) <- minImports
                       , let Just curRange = toCurrentRange posMapping range]
            return $ Right $ InL lens
          _ ->
            return $ Right $ InL []
    | otherwise =
      return $ Right $ InL []
  where generateLens :: Uri  -> Range -> Int -> CodeLens
        generateLens uri range int =
          CodeLens { _data_ = Just $ toJSON $ ResolveOne uri int
                   , _range = range
                   , _command = Nothing }

lensResolveProvider :: PluginMethodHandler IdeState Method_CodeLensResolve
lensResolveProvider ideState plId cl@(CodeLens {_data_ = Just data_}) = pluginResponse $ do
  case fromJSON data_ of
    Success (ResolveOne uri int) -> do
      nfp <- getNormalizedFilePath uri
      (MinimalImportsResult _ _ resolveMinImp) <-
        handleMaybeM "Unable to run Minimal Imports"
        $ liftIO
        $ runAction "MinimalImports" ideState $ use MinimalImports nfp
      target <- handleMaybe "Unable to resolve lens" $ resolveMinImp IM.!? int
      let updatedCodeLens = cl & L.command ?~  mkCommand plId uri target data_
      return updatedCodeLens
    _ -> throwE "unable to parse data_ field of CodeLens"
  where mkCommand ::  PluginId -> Uri -> (Range, T.Text) -> Value -> Command
        mkCommand pId uri (_, text) data_ =
          let title = abbreviateImportTitle text
              _arguments = Just [data_]
          in mkLspCommand pId importCommandId title _arguments

-- | If there are any implicit imports, provide one code action to turn them all
--   into explicit imports.
codeActionProvider :: (ModuleName -> Bool) -> PluginMethodHandler IdeState Method_TextDocumentCodeAction
codeActionProvider pred ideState _pId (CodeActionParams _ _ docId range _context)
  | TextDocumentIdentifier {_uri} <- docId,
    Just nfp <- uriToNormalizedFilePath $ toNormalizedUri _uri = pluginResponse $ do
    (MinimalImportsResult _ minImports _) <-
                  handleMaybeM "Unable to run Minimal Imports"
                    $ liftIO
                    $ runAction "MinimalImports" ideState $ use MinimalImports nfp
    let relevantCodeActions = filterByRange range minImports
        allExplicit =
          [InR $ mkCodeAction "Make all imports explicit" (Just $ toJSON $ ResolveAll _uri)
          | not $ null relevantCodeActions ]
        toCodeAction uri (range, int) =
          mkCodeAction "Make this import explicit" (Just $ toJSON $ ResolveOne uri int)
    pure $ InL ((InR . toCodeAction _uri <$> relevantCodeActions) <> allExplicit)
 | otherwise = return $ Right $ InL []
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

codeActionResolveProvider :: PluginMethodHandler IdeState Method_CodeActionResolve
codeActionResolveProvider ideState plId ca@(CodeAction{_data_= Just value}) =
  pluginResponse $ do
    case fromJSON value of
      Success (ResolveOne uri int) -> do
        nfp <- getNormalizedFilePath uri
        (MinimalImportsResult _ _ resolveMinImp) <-
          handleMaybeM "Unable to run Minimal Imports"
          $ liftIO
          $ runAction "MinimalImports" ideState $ use MinimalImports nfp
        (range, text) <- handleMaybe "Unable to resolve lens" $ resolveMinImp IM.!? int
        let wedit = mkWorkspaceEdit uri [TextEdit range text]
        pure $ ca & L.edit ?~ wedit
      Success (ResolveAll uri) -> do
        nfp <- getNormalizedFilePath uri
        (MinimalImportsResult _ _ resolveMinImp) <-
          handleMaybeM "Unable to run Minimal Imports"
          $ liftIO
          $ runAction "MinimalImports" ideState $ use MinimalImports nfp
        let edits =  uncurry TextEdit <$> IM.elems resolveMinImp
            wedit = mkWorkspaceEdit uri edits
        pure $ ca & L.edit ?~ wedit
  where mkWorkspaceEdit uri edits=
          WorkspaceEdit {_changes = Just $ Map.fromList [(uri, edits)]
                        , _documentChanges = Nothing
                        , _changeAnnotations = Nothing}
--------------------------------------------------------------------------------

data MinimalImports = MinimalImports
  deriving (Show, Generic, Eq, Ord)

instance Hashable MinimalImports

instance NFData MinimalImports

type instance RuleResult MinimalImports = MinimalImportsResult

data MinimalImportsResult = MinimalImportsResult
  { forLens        :: [(Range, Int)]
  , forCodeActions :: RM.RangeMap (Range, Int)
  , forResolve     :: IM.IntMap (Range, T.Text) }

instance Show MinimalImportsResult where show _ = "<minimalImportsResult>"

instance NFData MinimalImportsResult where rnf = rwhnf

data EIResolveData = ResolveOne
                      { uri :: Uri
                      , int :: Int }
                    | ResolveAll
                      { uri :: Uri }
                    deriving (Generic, ToJSON, FromJSON)

exportedModuleStrings :: ParsedModule -> [String]
exportedModuleStrings ParsedModule{pm_parsed_source = L _ HsModule{..}}
  | Just export <- hsmodExports,
    exports <- unLoc export
    = map (T.unpack . printOutputable) exports
exportedModuleStrings _ = []

minimalImportsRule :: Recorder (WithPriority Log) -> (ModuleName -> Bool) -> Rules ()
minimalImportsRule recorder pred = define (cmapWithPrio LogShake recorder) $ \MinimalImports nfp -> do
  -- Get the typechecking artifacts from the module
  Just tmr <- use TypeCheck nfp
  -- We also need a GHC session with all the dependencies
  Just hsc <- use GhcSessionDeps nfp
  -- Use the GHC api to extract the "minimal" imports
  (imports, Just mbMinImports) <- liftIO $ extractMinimalImports (Just hsc) (Just tmr)
  let importsMap =
        Map.fromList
          [ (realSrcSpanStart l, printOutputable i)
            | L (locA -> RealSrcSpan l _) i <- mbMinImports
            -- Don't we already take care of this with the predicate?
            --, not (isImplicitPrelude i)
          ]
      res =
        [ (realSrcSpanToRange l, minImport)
          | i@(L (locA -> src) imp) <- imports
          , not (isQualifiedImport imp)
          , not (isExplicitImport imp)
          , let L _ mn = ideclName imp
          , pred mn
          , let RealSrcSpan l _ = getLoc i
          , Just minImport <- [Map.lookup (realSrcSpanStart l) importsMap]
        ]
  uniques <- liftIO $ replicateM (length res) (U.hashUnique <$> U.newUnique)
  let uniqueAndRangeAndText = zip uniques res
      rangeAndUnique =  [ (r, u) | (u, (r, _)) <- uniqueAndRangeAndText ]
  return ([], Just $ MinimalImportsResult
                        rangeAndUnique
                        (RM.fromList fst rangeAndUnique)
                        (IM.fromList uniqueAndRangeAndText))
{-  where
     isImplicitPrelude :: (Outputable a) => a -> Bool
    isImplicitPrelude importDecl =
      T.isPrefixOf implicitPreludeImportPrefix (printOutputable importDecl)

-- | This is the prefix of an implicit prelude import which should be ignored,
-- when considering the minimal imports rule
implicitPreludeImportPrefix :: T.Text
implicitPreludeImportPrefix = "import (implicit) Prelude" -}

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

-- | Given an import declaration, generate a code lens unless it has an
-- explicit import list or it's qualified


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

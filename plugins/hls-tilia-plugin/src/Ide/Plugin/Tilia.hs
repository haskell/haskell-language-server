{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module Ide.Plugin.Tilia
  ( descriptor
  , provider
  , LogEvent
  )
where

import           Control.Exception                  (IOException, try)
import           Control.Exception.Safe             (tryAny)
import           Control.Monad.Except               (throwError)
import           Control.Monad.Extra                (firstJustM)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Except         (ExceptT (..), runExceptT)
import           Data.Choice                        (pattern Do, pattern Don't)
import           Data.Functor                       ((<&>))
import qualified Data.List.NonEmpty                 as NE
import qualified Data.Map.Strict                    as Map
import           Data.Maybe                         (fromMaybe, mapMaybe)
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Development.IDE                    hiding (pluginHandlers)
import           Development.IDE.Core.Compile       (setNonHomeFCHook)
import           Development.IDE.Core.PluginUtils   (mkFormattingHandlers)
import           Development.IDE.Import.FindImports (ModuleToFilenames (..))
import           GHC.Data.Maybe                     (MaybeErr (Succeeded))
import           GHC.Driver.Env                     (HscEnv,
                                                     hsc_all_home_unit_ids,
                                                     hsc_dflags, hsc_units)
import           GHC.Driver.Session                 (FlagSpec (..), homeUnitId_,
                                                     xFlags, xopt)
import           GHC.Iface.Load                     (WhereFrom (ImportBySystem),
                                                     loadInterface)
import           GHC.Tc.Utils.Monad                 (initIfaceLoad)
import qualified GHC.Types.Avail                    as GHC
import qualified GHC.Types.Fixity                   as GHC
import           GHC.Types.Name                     (Name, isBuiltInSyntax,
                                                     nameModule_maybe,
                                                     nameOccName)
import           GHC.Types.Name.Occurrence          (OccName, isTcClsNameSpace,
                                                     isTvNameSpace,
                                                     occNameSpace,
                                                     occNameString)
import           GHC.Types.PkgQual                  (PkgQual (NoPkgQual))
import           GHC.Types.Unique.Map               (lookupUniqMap)
import           GHC.Unit.Finder                    (FindResult (Found),
                                                     findImportedModule)
import           GHC.Unit.Info                      (unitPackageNameString,
                                                     unitPackageVersion)
import           GHC.Unit.Module                    (Definite (..),
                                                     GenUnit (RealUnit),
                                                     mkModule, mkModuleName,
                                                     moduleName,
                                                     moduleNameString)
import           GHC.Unit.Module.ModIface           (ModIface, mi_exports,
                                                     mi_fixities, mi_module)
import           GHC.Unit.State                     (LookupResult (..),
                                                     explicitUnits,
                                                     lookupModuleWithSuggestions,
                                                     lookupUnit)
import           GHC.Utils.Outputable               (text)
import           Ide.Plugin.Error                   (PluginError (PluginInternalError, PluginInvalidParams))
import           Ide.PluginUtils                    (makeDiffTextEdit)
import           Ide.Types                          hiding (Config)
import           Language.LSP.Protocol.Types
import           Language.LSP.Server                (ProgressCancellable (Cancellable))
import           System.FilePath                    (takeFileName)
import           System.Info                        (fullCompilerVersion)
import           Tilia.Editor                       (Compiled (..),
                                                     compiledSession,
                                                     editorSession,
                                                     formatBuffer)
import           Tilia.Fixity                       (Direction (..),
                                                     Fixity (..),
                                                     Namespace (..),
                                                     OpName (..))
import           Tilia.Fixity.HiFile                (HiExport (..), HiFile (..),
                                                     HiName (..))
import           Tilia.Format                       (describeFormatError)
import           Tilia.Palette                      (Palette (Plain))
import           Tilia.Pragma                       (lookupExtension)
import           Tilia.Run                          (Outcome (..))

-- ---------------------------------------------------------------------

descriptor :: Recorder (WithPriority LogEvent) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultPluginDescriptor plId desc)
    { pluginHandlers = mkFormattingHandlers $ provider recorder plId
    }
  where
    desc = "Provides formatting of Haskell files via tilia. Built with tilia-" <> VERSION_tilia

-- ---------------------------------------------------------------------

-- | Format a whole module with Tilia, which works out the project, the
-- extensions in force, and the fixities of operators from the file's path.
provider :: Recorder (WithPriority LogEvent) -> PluginId -> FormattingHandler IdeState
provider _ _ _ _ (FormatRange _) _ _ _ =
  throwError $ PluginInvalidParams "Tilia formats whole modules, not ranges"
provider recorder _ ideState token FormatText contents fp _ =
  ExceptT $ pluginWithIndefiniteProgress title token Cancellable $ \_updater -> runExceptT $ do
    logWith recorder Debug $ LogCompiledInVersion VERSION_tilia
    formatted <- libraryHandler
    pure $ InL $ maybe [] (makeDiffTextEdit contents) formatted
  where
    fp' = fromNormalizedFilePath fp
    title = T.pack $ "Formatting " <> takeFileName fp'

    libraryHandler :: ExceptT PluginError (HandlerM config) (Maybe Text)
    libraryHandler = do
      compiled <- liftIO $ compiledFor ideState fp
      logWith recorder Debug $ LogSessionFrom (maybe "Cabal" (const "HLS") compiled)
      outcome <- liftIO . try @IOException $
        session compiled
          >>= \case
            Left e        -> pure (Failed e)
            Right session -> formatBuffer session fp' contents
      case outcome of
        Left e -> throwError $ PluginInternalError $ "Tilia failed: " <> T.pack (show e)
        Right (Changed _ after) -> pure (Just after)
        Right Unchanged -> pure Nothing
        Right (Declined e) -> do
          logWith recorder Info $ LogDeclined (describeFormatError Plain e)
          pure Nothing
        Right (Failed e) -> throwError $ PluginInternalError $ describeFormatError Plain e

    session = \case
      Just compiled -> compiledSession fp' compiled (Don't #checkAst) (Don't #checkIdempotence)
      Nothing -> editorSession fp' Nothing (Do #useCache) (Do #download) (Don't #checkAst) (Don't #checkIdempotence) (Don't #debugFixity)

-- | What the session HLS keeps for the file's component knows about it, in
-- the terms Tilia asks for, so that Tilia need not ask Cabal or read the
-- sources and interfaces of dependencies itself.
compiledFor :: IdeState -> NormalizedFilePath -> IO (Maybe Compiled)
compiledFor ideState fp = do
  found <- runAction "Tilia" ideState $
    useWithStale GhcSessionDeps fp
      >>= maybe (use GhcSession fp) (pure . Just . fst)
  pure (compiledIn ideState fp . hscEnv <$> found)

-- | What an 'HscEnv' knows about its component, in the terms Tilia asks for.
compiledIn :: IdeState -> NormalizedFilePath -> HscEnv -> Compiled
compiledIn ideState fp env =
  Compiled
    { compiledExtensions =
        mapMaybe
          (lookupExtension . T.pack . flagSpecName)
          [f | f <- xFlags, xopt (flagSpecFlag f) (hsc_dflags env)]
    , compiledWith = fullCompilerVersion
    , compiledPackages =
        Map.fromList
          [ (T.pack (unitPackageNameString info), unitPackageVersion info)
          | (unit, _) <- explicitUnits units
          , Just info <- [lookupUnit units unit]
          ]
    , compiledInterface = interfaceIn ideState fp env
    }
  where
    units = hsc_units env

-- | The interface of a module of the project, compiled already or compiled
-- now, of a module the component can import from a package, or of one in a
-- package it cannot import, which is where a reexported name may be
-- declared, such as @ghc-internal@ for @base@.
interfaceIn :: IdeState -> NormalizedFilePath -> HscEnv -> Text -> IO (Maybe HiFile)
interfaceIn ideState fp env name =
  fmap (either (const Nothing) id) . tryAny $
    firstJustM
      id
      [ firstJustM loaded inSession
      , compiledNow
      , firstJustM loaded =<< inPackages
      ]
  where
    env' = setNonHomeFCHook env
    modName = mkModuleName (T.unpack name)
    inSession =
      [ mkModule (RealUnit (Definite unit)) modName
      | unit <- Set.toList (hsc_all_home_unit_ids env)
      ]
    compiledNow = runAction "Tilia" ideState $
      use GetModulesPaths fp >>= \case
        Just paths
          | Just providers <- lookupUniqMap (moduleMap paths) modName ->
              fmap (hiFileOf . hirModIface . fst)
                <$> useWithStale GetModIface (ownFirst providers)
        _ -> pure Nothing
    ownFirst providers =
      fromMaybe
        (snd (NE.head providers))
        (lookup (homeUnitId_ (hsc_dflags env)) (NE.toList providers))
    inPackages = do
      visible <- findImportedModule env' modName NoPkgQual
      pure $
        [m | Found _ m <- [visible]]
          <> case lookupModuleWithSuggestions (hsc_units env) modName NoPkgQual of
            LookupFound m _ -> [m]
            LookupMultiple ms -> fmap fst ms
            LookupHidden inHiddenUnits hidden -> fmap fst (inHiddenUnits <> hidden)
            _ -> []
    loaded m =
      initIfaceLoad env' (loadInterface (text "Tilia") m ImportBySystem)
        <&> \case
          Succeeded iface -> Just (hiFileOf iface)
          _ -> Nothing

-- | A module's interface as Tilia reads it.
hiFileOf :: ModIface -> HiFile
hiFileOf iface =
  HiFile
    { hiModule = T.pack (moduleNameString (moduleName (mi_module iface)))
    , hiExports = fmap exported (mi_exports iface)
    , hiFixities =
        [ (namespaceOf occ, opName occ, fixityOf f)
        | (occ, f) <- mi_fixities iface
        ]
    }
  where
    exported = \case
      GHC.Avail n      -> Avail (hiName n)
      GHC.AvailTC n ns -> AvailTC (hiName n) (fmap hiName ns)
    fixityOf (GHC.Fixity precedence direction) =
      Fixity
        ( case direction of
            GHC.InfixL -> LeftAssoc
            GHC.InfixR -> RightAssoc
            GHC.InfixN -> NoAssoc
        )
        precedence

-- | A name an interface refers to, as Tilia reads it.
hiName :: Name -> HiName
hiName n
  | isBuiltInSyntax n = BuiltInSyntax (namespaceOf occ) (opName occ)
  | Just m <- nameModule_maybe n =
      HiName (T.pack (moduleNameString (moduleName m))) (namespaceOf occ) (opName occ)
  | otherwise = Unneeded
  where
    occ = nameOccName n

-- | The namespace a fixity declaration for this name governs.
namespaceOf :: OccName -> Namespace
namespaceOf occ
  | isTcClsNameSpace space || isTvNameSpace space = InTypes
  | otherwise = InTerms
  where
    space = occNameSpace occ

-- | A name as Tilia spells it.
opName :: OccName -> OpName
opName = OpName . T.pack . occNameString

data LogEvent
    = LogDeclined Text
    | LogCompiledInVersion String
    | LogSessionFrom Text
    deriving (Show)

instance Pretty LogEvent where
    pretty = \case
        LogDeclined t -> "Tilia declined to format:" <> line <> indent 2 (pretty t)
        LogCompiledInVersion v -> "Using compiled in tilia-" <> pretty v
        LogSessionFrom from -> "Working out the project with" <+> pretty from

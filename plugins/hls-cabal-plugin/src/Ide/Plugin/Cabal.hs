{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module Ide.Plugin.Cabal (descriptor, Log(..)) where

import           Control.Concurrent.STM
import           Control.Concurrent.Strict
import           Control.DeepSeq
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import qualified Data.ByteString                 as BS
import           Data.Hashable
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as HashMap
import qualified Data.List.NonEmpty              as NE
import qualified Data.Text.Encoding              as Encoding
import           Data.Typeable
import           Development.IDE                 as D
import           Development.IDE.Core.Shake      (restartShakeSession)
import qualified Development.IDE.Core.Shake      as Shake
import           Development.IDE.Graph           (alwaysRerun)
import           GHC.Generics
import qualified Ide.Plugin.Cabal.Diagnostics    as Diagnostics
import qualified Ide.Plugin.Cabal.LicenseSuggest as LicenseSuggest
import qualified Ide.Plugin.Cabal.Parse          as Parse
import           Ide.Plugin.Config               (Config)
import           Ide.Types
import           Language.LSP.Server             (LspM)
import           Language.LSP.Types
import qualified Language.LSP.Types              as LSP
import qualified Language.LSP.VFS                as VFS

data Log
  = LogModificationTime NormalizedFilePath FileVersion
  | LogShake Shake.Log
  | LogDocOpened Uri
  | LogDocModified Uri
  | LogDocSaved Uri
  | LogDocClosed Uri
  | LogFOI (HashMap NormalizedFilePath FileOfInterestStatus)
  deriving Show

instance Pretty Log where
  pretty = \case
    LogShake log' -> pretty log'
    LogModificationTime nfp modTime  ->
      "Modified:" <+> pretty (fromNormalizedFilePath nfp) <+> pretty (show modTime)
    LogDocOpened uri ->
      "Opened text document:" <+> pretty (getUri uri)
    LogDocModified uri ->
      "Modified text document:" <+> pretty (getUri uri)
    LogDocSaved uri ->
      "Saved text document:" <+> pretty (getUri uri)
    LogDocClosed uri ->
      "Closed text document:" <+> pretty (getUri uri)
    LogFOI files ->
      "Set files of interest to:" <+> viaShow files


descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultCabalPluginDescriptor plId)
  { pluginRules = cabalRules recorder
  , pluginHandlers = mkPluginHandler STextDocumentCodeAction licenseSuggestCodeAction
  , pluginNotificationHandlers = mconcat
  [ mkPluginNotificationHandler LSP.STextDocumentDidOpen $
      \ide vfs _ (DidOpenTextDocumentParams TextDocumentItem{_uri,_version}) -> liftIO $ do
      whenUriFile _uri $ \file -> do
        log' Debug $ LogDocOpened _uri
        addFileOfInterest recorder ide file Modified{firstOpen=True}
        restartCabalShakeSession (shakeExtras ide) vfs file "(opened)"

  , mkPluginNotificationHandler LSP.STextDocumentDidChange $
      \ide vfs _ (DidChangeTextDocumentParams VersionedTextDocumentIdentifier{_uri} _) -> liftIO $ do
      whenUriFile _uri $ \file -> do
        log' Debug $ LogDocModified _uri
        addFileOfInterest recorder ide file Modified{firstOpen=False}
        restartCabalShakeSession (shakeExtras ide) vfs file "(changed)"

  , mkPluginNotificationHandler LSP.STextDocumentDidSave $
      \ide vfs _ (DidSaveTextDocumentParams TextDocumentIdentifier{_uri} _) -> liftIO $ do
      whenUriFile _uri $ \file -> do
        log' Debug $ LogDocSaved _uri
        addFileOfInterest recorder ide file OnDisk
        restartCabalShakeSession (shakeExtras ide) vfs file "(saved)"

  , mkPluginNotificationHandler LSP.STextDocumentDidClose $
      \ide vfs _ (DidCloseTextDocumentParams TextDocumentIdentifier{_uri}) -> liftIO $ do
      whenUriFile _uri $ \file -> do
        log' Debug $ LogDocClosed _uri
        deleteFileOfInterest recorder ide file
        restartCabalShakeSession (shakeExtras ide) vfs file "(closed)"
  ]
  }
  where
    log' = logWith recorder

    whenUriFile :: Uri -> (NormalizedFilePath -> IO ()) -> IO ()
    whenUriFile uri act = whenJust (LSP.uriToFilePath uri) $ act . toNormalizedFilePath'

-- | Helper function to restart the shake session, specifically for modifying .cabal files.
-- No special logic, just group up a bunch of functions you need for the base
-- Notification Handlers.
--
-- To make sure diagnostics are up to date, we need to tell shake that the file was touched and
-- needs to be re-parsed. That's what we do when we record the dirty key that our parsing
-- rule depends on.
-- Then we restart the shake session, so that changes to our virtual files are actually picked up.
restartCabalShakeSession :: ShakeExtras -> VFS.VFS -> NormalizedFilePath -> String -> IO ()
restartCabalShakeSession shakeExtras vfs file actionMsg = do
  join $ atomically $ Shake.recordDirtyKeys shakeExtras GetModificationTime [file]
  restartShakeSession shakeExtras (VFSModified vfs) (fromNormalizedFilePath file ++ " " ++ actionMsg) []

-- ----------------------------------------------------------------
-- Plugin Rules
-- ----------------------------------------------------------------

data ParseCabal = ParseCabal
    deriving (Eq, Show, Typeable, Generic)
instance Hashable ParseCabal
instance NFData   ParseCabal

type instance RuleResult ParseCabal = ()

cabalRules :: Recorder (WithPriority Log) -> Rules ()
cabalRules recorder = do
  -- Make sure we initialise the cabal files-of-interest.
  ofInterestRules recorder
  -- Rule to produce diagnostics for cabal files.
  define (cmapWithPrio LogShake recorder) $ \ParseCabal file -> do
    -- whenever this key is marked as dirty (e.g., when a user writes stuff to it),
    -- we rerun this rule because this rule *depends* on GetModificationTime.
    (t, mCabalSource) <- use_ GetFileContents file
    log' Debug $ LogModificationTime file t
    contents <- case mCabalSource of
      Just sources -> pure $ Encoding.encodeUtf8 sources
      Nothing -> do
        liftIO $ BS.readFile $ fromNormalizedFilePath file

    (pWarnings, pm) <- liftIO $ Parse.parseCabalFileContents contents
    let warningDiags = fmap (Diagnostics.warningDiagnostic file) pWarnings
    case pm of
      Left (_cabalVersion, pErrorNE) -> do
        let errorDiags = NE.toList $ NE.map (Diagnostics.errorDiagnostic file) pErrorNE
            allDiags = errorDiags <> warningDiags
        pure (allDiags, Nothing)
      Right _ -> do
        pure (warningDiags, Just ())

  action $ do
    -- Run the cabal kick. This code always runs when 'shakeRestart' is run.
    -- Must be careful to not impede the performance too much. Crucial to
    -- a snappy IDE experience.
    kick
  where
    log' = logWith recorder

-- | This is the kick function for the cabal plugin.
-- We run this action, whenever we shake session us run/restarted, which triggers
-- actions to produce diagnostics for cabal files.
--
-- It is paramount that this kick-function can be run quickly, since it is a blocking
-- function invocation.
kick :: Action ()
kick = do
  files <- HashMap.keys <$> getCabalFilesOfInterestUntracked
  void $ uses ParseCabal files

-- ----------------------------------------------------------------
-- Code Actions
-- ----------------------------------------------------------------

licenseSuggestCodeAction
  :: IdeState
  -> PluginId
  -> CodeActionParams
  -> LspM Config (Either ResponseError (ResponseResult 'TextDocumentCodeAction))
licenseSuggestCodeAction _ _ (CodeActionParams _ _ (TextDocumentIdentifier uri) _range CodeActionContext{_diagnostics=List diags}) =
  pure $ Right $ List $ diags >>= (fmap InR . (LicenseSuggest.licenseErrorAction uri))

-- ----------------------------------------------------------------
-- Cabal file of Interest rules and global variable
-- ----------------------------------------------------------------

-- | Cabal files that are currently open in the lsp-client.
-- Specific actions happen when these files are saved, closed or modified,
-- such as generating diagnostics, re-parsing, etc...
--
-- We need to store the open files to parse them again if we restart the shake session.
-- Restarting of the shake session happens whenever these files are modified.
newtype OfInterestCabalVar = OfInterestCabalVar (Var (HashMap NormalizedFilePath FileOfInterestStatus))

instance Shake.IsIdeGlobal OfInterestCabalVar

data IsCabalFileOfInterest = IsCabalFileOfInterest
    deriving (Eq, Show, Typeable, Generic)
instance Hashable IsCabalFileOfInterest
instance NFData   IsCabalFileOfInterest

type instance RuleResult IsCabalFileOfInterest = CabalFileOfInterestResult

data CabalFileOfInterestResult = NotCabalFOI | IsCabalFOI FileOfInterestStatus
  deriving (Eq, Show, Typeable, Generic)
instance Hashable CabalFileOfInterestResult
instance NFData   CabalFileOfInterestResult

-- | The rule that initialises the files of interest state.
--
-- Needs to be run on start-up.
ofInterestRules :: Recorder (WithPriority Log) -> Rules ()
ofInterestRules recorder = do
    Shake.addIdeGlobal . OfInterestCabalVar =<< liftIO (newVar HashMap.empty)
    Shake.defineEarlyCutoff (cmapWithPrio LogShake recorder) $ RuleNoDiagnostics $ \IsCabalFileOfInterest f -> do
        alwaysRerun
        filesOfInterest <- getCabalFilesOfInterestUntracked
        let foi = maybe NotCabalFOI IsCabalFOI $ f `HashMap.lookup` filesOfInterest
            fp  = summarize foi
            res = (Just fp, Just foi)
        return res
    where
    summarize NotCabalFOI                   = BS.singleton 0
    summarize (IsCabalFOI OnDisk)           = BS.singleton 1
    summarize (IsCabalFOI (Modified False)) = BS.singleton 2
    summarize (IsCabalFOI (Modified True))  = BS.singleton 3

getCabalFilesOfInterestUntracked :: Action (HashMap NormalizedFilePath FileOfInterestStatus)
getCabalFilesOfInterestUntracked = do
    OfInterestCabalVar var <- Shake.getIdeGlobalAction
    liftIO $ readVar var

addFileOfInterest :: Recorder (WithPriority Log) -> IdeState -> NormalizedFilePath -> FileOfInterestStatus -> IO ()
addFileOfInterest recorder state f v = do
    OfInterestCabalVar var <- Shake.getIdeGlobalState state
    (prev, files) <- modifyVar var $ \dict -> do
        let (prev, new) = HashMap.alterF (, Just v) f dict
        pure (new, (prev, new))
    when (prev /= Just v) $ do
        join $ atomically $ Shake.recordDirtyKeys (shakeExtras state) IsFileOfInterest [f]
        log' Debug $ LogFOI files
  where
    log' = logWith recorder

deleteFileOfInterest :: Recorder (WithPriority Log) -> IdeState -> NormalizedFilePath -> IO ()
deleteFileOfInterest recorder state f = do
    OfInterestCabalVar var <- Shake.getIdeGlobalState state
    files <- modifyVar' var $ HashMap.delete f
    join $ atomically $ Shake.recordDirtyKeys (shakeExtras state) IsFileOfInterest [f]
    log' Debug $ LogFOI files
  where
    log' = logWith recorder

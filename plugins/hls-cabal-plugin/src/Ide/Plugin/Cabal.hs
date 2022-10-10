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
import           Data.Maybe                      (mapMaybe)
import qualified Data.Text                       as T
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
  = LogModificationTime NormalizedFilePath (Maybe FileVersion)
  | LogDiagnostics NormalizedFilePath [FileDiagnostic]
  | LogShake Shake.Log
  | LogDocOpened Uri
  | LogDocModified Uri
  | LogDocSaved Uri
  | LogDocClosed Uri
  deriving Show

instance Pretty Log where
  pretty = \case
    LogShake log' -> pretty log'
    LogModificationTime nfp modTime  ->
      "Modified:" <+> pretty (fromNormalizedFilePath nfp) <+> pretty (show modTime)
    LogDiagnostics nfp diags ->
      "Diagnostics for" <+> pretty (fromNormalizedFilePath nfp) <> ":" <+> pretty (show diags)
    LogDocOpened uri ->
      "Opened text document:" <+> pretty (getUri uri)
    LogDocModified uri ->
      "Modified text document:" <+> pretty (getUri uri)
    LogDocSaved uri ->
      "Saved text document:" <+> pretty (getUri uri)
    LogDocClosed uri ->
      "Closed text document:" <+> pretty (getUri uri)

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultCabalPluginDescriptor plId)
  { pluginRules = cabalRules recorder
  , pluginHandlers = mkPluginHandler STextDocumentCodeAction licenseSuggestCodeAction
  , pluginNotificationHandlers = mconcat
  [ mkPluginNotificationHandler LSP.STextDocumentDidOpen $
      \ide vfs _ (DidOpenTextDocumentParams TextDocumentItem{_uri,_version}) -> liftIO $ do
      whenUriFile _uri $ \file -> do
        log' Debug $ LogDocOpened _uri
        addFileOfInterest ide file Modified{firstOpen=True}
        restartCabalShakeSession ide vfs file "(opened)"

  , mkPluginNotificationHandler LSP.STextDocumentDidChange $
      \ide vfs _ (DidChangeTextDocumentParams VersionedTextDocumentIdentifier{_uri} _) -> liftIO $ do
      whenUriFile _uri $ \file -> do
        log' Debug $ LogDocModified _uri
        addFileOfInterest ide file Modified{firstOpen=False}
        restartCabalShakeSession ide vfs file "(changed)"

  , mkPluginNotificationHandler LSP.STextDocumentDidSave $
      \ide vfs _ (DidSaveTextDocumentParams TextDocumentIdentifier{_uri} _) -> liftIO $ do
      whenUriFile _uri $ \file -> do
        log' Debug $ LogDocSaved _uri
        addFileOfInterest ide file OnDisk
        restartCabalShakeSession ide vfs file "(saved)"

  , mkPluginNotificationHandler LSP.STextDocumentDidClose $
      \ide vfs _ (DidCloseTextDocumentParams TextDocumentIdentifier{_uri}) -> liftIO $ do
      whenUriFile _uri $ \file -> do
        log' Debug $ LogDocClosed _uri
        deleteFileOfInterest ide file
        restartCabalShakeSession ide vfs file "(closed)"
  ]
  }
  where
    log' = logWith recorder

    whenUriFile :: Uri -> (NormalizedFilePath -> IO ()) -> IO ()
    whenUriFile uri act = whenJust (LSP.uriToFilePath uri) $ act . toNormalizedFilePath'

-- | Helper function to restart the shake session, specifically for modifying .cabal files.
-- No special logic, just group up a bunch of functions you need for the base
-- Notification Handlers.
restartCabalShakeSession :: IdeState -> VFS.VFS -> NormalizedFilePath -> String -> IO ()
restartCabalShakeSession ide vfs file actionMsg = do
  join $ atomically $ Shake.recordDirtyKeys (shakeExtras ide) GetModificationTime [file]
  restartShakeSession (shakeExtras ide) (VFSModified vfs) (fromNormalizedFilePath file ++ " " ++ actionMsg) []

-- ----------------------------------------------------------------
-- Cabal file of Interset rules and global variable
-- ----------------------------------------------------------------

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

getCabalFilesOfInterestUntracked ::  Action (HashMap NormalizedFilePath FileOfInterestStatus)
getCabalFilesOfInterestUntracked = do
    OfInterestCabalVar var <- Shake.getIdeGlobalAction
    liftIO $ readVar var

getFilesOfInterest :: IdeState -> IO( HashMap NormalizedFilePath FileOfInterestStatus)
getFilesOfInterest state = do
    OfInterestCabalVar var <- Shake.getIdeGlobalState state
    readVar var

addFileOfInterest :: IdeState -> NormalizedFilePath -> FileOfInterestStatus -> IO ()
addFileOfInterest state f v = do
    OfInterestCabalVar var <- Shake.getIdeGlobalState state
    (prev, files) <- modifyVar var $ \dict -> do
        let (prev, new) = HashMap.alterF (, Just v) f dict
        pure (new, (prev, new))
    when (prev /= Just v) $ do
        join $ atomically $ Shake.recordDirtyKeys (shakeExtras state) IsFileOfInterest [f]
        logDebug (ideLogger state) $
            "Set files of interest to: " <> T.pack (show files)

deleteFileOfInterest :: IdeState -> NormalizedFilePath -> IO ()
deleteFileOfInterest state f = do
    OfInterestCabalVar var <- Shake.getIdeGlobalState state
    files <- modifyVar' var $ HashMap.delete f
    join $ atomically $ Shake.recordDirtyKeys (shakeExtras state) IsFileOfInterest [f]
    logDebug (ideLogger state) $ "Set files of interest to: " <> T.pack (show files)

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
  ofInterestRules recorder
  define (cmapWithPrio LogShake recorder) $ \ParseCabal file -> do
    t <- use GetModificationTime file
    log' Debug $ LogModificationTime file t
    mVirtualFile <- Shake.getVirtualFile file
    contents <- case mVirtualFile of
      Just vfile -> pure $ Encoding.encodeUtf8 $ VFS.virtualFileText vfile
      Nothing -> do
        liftIO $ BS.readFile $ fromNormalizedFilePath file

    (pWarnings, pm) <- liftIO $ Parse.parseCabalFileContents contents
    let warningDiags = fmap (Diagnostics.warningDiagnostic file) pWarnings
    case pm of
      Left (_cabalVersion, pErrorNE) -> do
        let errorDiags = NE.toList $ NE.map (Diagnostics.errorDiagnostic file) pErrorNE
            allDiags = errorDiags <> warningDiags
        log' Debug $ LogDiagnostics file allDiags
        pure (allDiags, Nothing)
      Right _ -> do
        log' Debug $ LogDiagnostics file warningDiags
        pure (warningDiags, Just ())

  action $ do
    -- Run the cabal kick. This code always runs when 'shakeRestart' is run.
    -- Must be careful to not impede the performance too much. Crucial to
    -- a snappy IDE experience.
    kick
  where
    log' = logWith recorder

-- | TODO: add documentation
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
  pure $ Right $ List $ mapMaybe (fmap InR . LicenseSuggest.licenseErrorAction uri) diags

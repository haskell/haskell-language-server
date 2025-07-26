{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Ide.Plugin.CabalProject where

import           Control.Concurrent.Strict
import           Control.DeepSeq
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import qualified Data.ByteString                     as BS
import           Data.Hashable
import           Data.HashMap.Strict                 (HashMap)
import qualified Data.HashMap.Strict                 as HashMap
import qualified Data.List.NonEmpty                  as NE
import           Data.Proxy
import qualified Data.Text                           ()
import qualified Data.Text.Encoding                  as Encoding
import           Data.Text.Utf16.Rope.Mixed          as Rope
import           Development.IDE                     as D
import           Development.IDE.Core.Shake          (restartShakeSession)
import qualified Development.IDE.Core.Shake          as Shake
import           Development.IDE.Graph               (Key, alwaysRerun)
import           Development.IDE.Types.Shake         (toKey)
import           GHC.Generics
import           Ide.Plugin.Cabal.Orphans            ()
import           Ide.Plugin.CabalProject.Diagnostics as Diagnostics
import           Ide.Plugin.CabalProject.Parse       as Parse
import           Ide.Plugin.CabalProject.Types       as Types
import           Ide.Types
import qualified Language.LSP.Protocol.Message       as LSP
import           Language.LSP.Protocol.Types
import qualified Language.LSP.VFS                    as VFS

data Log
  = LogModificationTime NormalizedFilePath FileVersion
  | LogShake Shake.Log
  | LogDocOpened Uri
  | LogDocModified Uri
  | LogDocSaved Uri
  | LogDocClosed Uri
  | LogFOI (HashMap NormalizedFilePath FileOfInterestStatus)
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogShake log' -> pretty log'
    LogModificationTime nfp modTime ->
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
descriptor recorder plId =
  (defaultCabalProjectPluginDescriptor plId "Provides a variety of IDE features in cabal.project files")
    { pluginRules = cabalProjectRules recorder plId
    , pluginHandlers =
        mconcat
          []
    , pluginNotificationHandlers =
        mconcat
          [ mkPluginNotificationHandler LSP.SMethod_TextDocumentDidOpen $
              \ide vfs _ (DidOpenTextDocumentParams TextDocumentItem{_uri, _version}) -> liftIO $ do
                whenUriFile _uri $ \file -> do
                  log' Debug $ LogDocOpened _uri
                  restartCabalProjectShakeSession (shakeExtras ide) vfs file "(opened)" $
                    addFileOfInterest recorder ide file Modified{firstOpen = True}
          , mkPluginNotificationHandler LSP.SMethod_TextDocumentDidChange $
              \ide vfs _ (DidChangeTextDocumentParams VersionedTextDocumentIdentifier{_uri} _) -> liftIO $ do
                whenUriFile _uri $ \file-> do
                  log' Debug $ LogDocModified _uri
                  restartCabalProjectShakeSession (shakeExtras ide) vfs file "(changed)" $
                    addFileOfInterest recorder ide file Modified{firstOpen = False}
          , mkPluginNotificationHandler LSP.SMethod_TextDocumentDidSave $
              \ide vfs _ (DidSaveTextDocumentParams TextDocumentIdentifier{_uri} _) -> liftIO $ do
                whenUriFile _uri $ \file -> do
                  log' Debug $ LogDocSaved _uri
                  restartCabalProjectShakeSession (shakeExtras ide) vfs file "(saved)" $
                    addFileOfInterest recorder ide file OnDisk
          , mkPluginNotificationHandler LSP.SMethod_TextDocumentDidClose $
              \ide vfs _ (DidCloseTextDocumentParams TextDocumentIdentifier{_uri}) -> liftIO $ do
                whenUriFile _uri $ \file -> do
                  log' Debug $ LogDocClosed _uri
                  restartCabalProjectShakeSession (shakeExtras ide) vfs file "(closed)" $
                    deleteFileOfInterest recorder ide file
          ]
    , pluginConfigDescriptor = defaultConfigDescriptor
      { configHasDiagnostics = True
      }
    }
 where
  log' = logWith recorder

  whenUriFile :: Uri -> (NormalizedFilePath -> IO ()) -> IO ()
  whenUriFile uri act = whenJust (uriToFilePath uri) $ act . toNormalizedFilePath'

{- | Helper function to restart the shake session, specifically for modifying cabal.project files.
No special logic, just group up a bunch of functions you need for the base
Notification Handlers.

To make sure diagnostics are up to date, we need to tell shake that the file was touched and
needs to be re-parsed. That's what we do when we record the dirty key that our parsing
rule depends on.
Then we restart the shake session, so that changes to our virtual files are actually picked up.
-}
restartCabalProjectShakeSession :: ShakeExtras -> VFS.VFS -> NormalizedFilePath -> String -> IO [Key] -> IO ()
restartCabalProjectShakeSession shakeExtras vfs file actionMsg actionBetweenSession = do
  restartShakeSession shakeExtras (VFSModified vfs) (fromNormalizedFilePath file ++ " " ++ actionMsg) [] $ do
    keys <- actionBetweenSession
    return (toKey GetModificationTime file:keys)


cabalProjectRules :: Recorder (WithPriority Log) -> PluginId -> Rules ()
cabalProjectRules recorder plId = do
  -- Make sure we initialise the cabal project files-of-interest.
  ofInterestRules recorder
  -- Rule to produce diagnostics for cabal project files.
  define (cmapWithPrio LogShake recorder) $ \ParseCabalProjectFields file -> do
    config <- getPluginConfigAction plId
    if not (plcGlobalOn config && plcDiagnosticsOn config)
      then pure ([], Nothing)
      else do
        -- whenever this key is marked as dirty (e.g., when a user writes stuff to it),
        -- we rerun this rule because this rule *depends* on GetModificationTime.
        (t, mCabalProjectSource) <- use_ GetFileContents file
        log' Debug $ LogModificationTime file t
        contents <- case mCabalProjectSource of
          Just sources ->
            pure $ Encoding.encodeUtf8 $ Rope.toText sources
          Nothing -> do
            liftIO $ BS.readFile $ fromNormalizedFilePath file

        case Parse.readCabalProjectFields file contents of
          Left _ ->
            pure ([], Nothing)
          Right fields ->
            pure ([], Just fields)

  define (cmapWithPrio LogShake recorder) $ \ParseCabalProjectFile file -> do
    cfg <- getPluginConfigAction plId
    if not (plcGlobalOn cfg && plcDiagnosticsOn cfg)
      then pure ([], Nothing)
      else do
        -- whenever this key is marked as dirty (e.g., when a user writes stuff to it),
        -- we rerun this rule because this rule *depends* on GetModificationTime.
        (t, mCabalProjectSource) <- use_ GetFileContents file
        log' Debug $ LogModificationTime file t

        contents <- case mCabalProjectSource of
          Just sources ->
            pure $ Encoding.encodeUtf8 $ Rope.toText sources
          Nothing      ->
            liftIO $ BS.readFile $ fromNormalizedFilePath file

        (pWarnings, pResult) <- liftIO $ Parse.parseCabalProjectFileContents (fromNormalizedFilePath file) contents
        let warnDiags = fmap (Diagnostics.warningDiagnostic file) pWarnings

        case pResult of
          Left (_specVer, pErrNE) -> do
            let errDiags = NE.toList $ NE.map (Diagnostics.errorDiagnostic file) pErrNE
            pure (errDiags ++ warnDiags, Nothing)

          Right projCfg -> do
            pure (warnDiags, Just projCfg)

  action $ do
    -- Run the cabal project kick. This code always runs when 'shakeRestart' is run.
    -- Must be careful to not impede the performance too much. Crucial to
    -- a snappy IDE experience.
    kick
 where
  log' = logWith recorder

{- | This is the kick function for the cabal project plugin.
We run this action, whenever we shake session us run/restarted, which triggers
actions to produce diagnostics for cabal project files.

It is paramount that this kick-function can be run quickly, since it is a blocking
function invocation.
-}
kick :: Action ()
kick = do
  files <- HashMap.keys <$> getCabalProjectFilesOfInterestUntracked
  Shake.runWithSignal (Proxy @"kick/start/cabal-project") (Proxy @"kick/done/cabal-project") files Types.ParseCabalProjectFile


-- ----------------------------------------------------------------
-- Cabal project file of Interest rules and global variable
-- ----------------------------------------------------------------

{- | Cabal project files that are currently open in the lsp-client.
Specific actions happen when these files are saved, closed or modified,
such as generating diagnostics, re-parsing, etc...

We need to store the open files to parse them again if we restart the shake session.
Restarting of the shake session happens whenever these files are modified.
-}
newtype OfInterestCabalProjectVar = OfInterestCabalProjectVar (Var (HashMap NormalizedFilePath FileOfInterestStatus))

instance Shake.IsIdeGlobal OfInterestCabalProjectVar

data IsCabalProjectFileOfInterest = IsCabalProjectFileOfInterest
  deriving (Eq, Show, Generic)
instance Hashable IsCabalProjectFileOfInterest
instance NFData IsCabalProjectFileOfInterest

type instance RuleResult IsCabalProjectFileOfInterest = CabalProjectFileOfInterestResult

data CabalProjectFileOfInterestResult = NotCabalProjectFOI | IsCabalProjectFOI FileOfInterestStatus
  deriving (Eq, Show, Generic)
instance Hashable CabalProjectFileOfInterestResult
instance NFData CabalProjectFileOfInterestResult

{- | The rule that initialises the files of interest state.

Needs to be run on start-up.
-}
ofInterestRules :: Recorder (WithPriority Log) -> Rules ()
ofInterestRules recorder = do
  Shake.addIdeGlobal . OfInterestCabalProjectVar =<< liftIO (newVar HashMap.empty)
  Shake.defineEarlyCutoff (cmapWithPrio LogShake recorder) $ RuleNoDiagnostics $ \IsCabalProjectFileOfInterest f -> do
    alwaysRerun
    filesOfInterest <- getCabalProjectFilesOfInterestUntracked
    let foi = maybe NotCabalProjectFOI IsCabalProjectFOI $ f `HashMap.lookup` filesOfInterest
        fp = summarize foi
        res = (Just fp, Just foi)
    return res
 where
  summarize NotCabalProjectFOI                   = BS.singleton 0
  summarize (IsCabalProjectFOI OnDisk)           = BS.singleton 1
  summarize (IsCabalProjectFOI (Modified False)) = BS.singleton 2
  summarize (IsCabalProjectFOI (Modified True))  = BS.singleton 3

getCabalProjectFilesOfInterestUntracked :: Action (HashMap NormalizedFilePath FileOfInterestStatus)
getCabalProjectFilesOfInterestUntracked = do
  OfInterestCabalProjectVar var <- Shake.getIdeGlobalAction
  liftIO $ readVar var

addFileOfInterest :: Recorder (WithPriority Log) -> IdeState -> NormalizedFilePath -> FileOfInterestStatus -> IO [Key]
addFileOfInterest recorder state f v = do
  OfInterestCabalProjectVar var <- Shake.getIdeGlobalState state
  (prev, files) <- modifyVar var $ \dict -> do
    let (prev, new) = HashMap.alterF (,Just v) f dict
    pure (new, (prev, new))
  if prev /= Just v
    then do
        log' Debug $ LogFOI files
        return [toKey IsCabalProjectFileOfInterest f]
    else return []
 where
  log' = logWith recorder

deleteFileOfInterest :: Recorder (WithPriority Log) -> IdeState -> NormalizedFilePath -> IO [Key]
deleteFileOfInterest recorder state f = do
  OfInterestCabalProjectVar var <- Shake.getIdeGlobalState state
  files <- modifyVar' var $ HashMap.delete f
  log' Debug $ LogFOI files
  return [toKey IsFileOfInterest f]
 where
  log' = logWith recorder

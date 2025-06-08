{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.CabalFmt where

import           Control.Lens
import           Control.Monad.Except             (throwError)
import           Control.Monad.IO.Class
import qualified Data.Text                        as T
import           Development.IDE                  hiding (pluginHandlers)
import           Development.IDE.Core.PluginUtils (mkFormattingHandlers)
import           Ide.Plugin.Error                 (PluginError (PluginInternalError, PluginInvalidParams))
import           Ide.Plugin.Properties
import           Ide.PluginUtils
import           Ide.Types
import qualified Language.LSP.Protocol.Lens       as L
import           Language.LSP.Protocol.Types
import           Prelude                          hiding (log)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process.ListLike
import qualified System.Process.Text              as Process

data Log
  = LogProcessInvocationFailure Int
  | LogReadCreateProcessInfo T.Text [String]
  | LogInvalidInvocationInfo
  | LogFormatterBinNotFound FilePath
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogProcessInvocationFailure exitCode -> "Invocation of cabal-fmt failed with code" <+> pretty exitCode
    LogReadCreateProcessInfo stdErrorOut args ->
      vcat $
        ["Invocation of cabal-fmt with arguments" <+> pretty args]
          ++ ["failed with standard error:" <+> pretty stdErrorOut | not (T.null stdErrorOut)]
    LogInvalidInvocationInfo -> "Invocation of cabal-fmt with range was called but is not supported."
    LogFormatterBinNotFound fp -> "Couldn't find formatter executable 'cabal-fmt' at:" <+> pretty fp

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultCabalPluginDescriptor plId "Provides formatting of cabal files with cabal-fmt")
    { pluginHandlers = mkFormattingHandlers (provider recorder plId)
    , pluginConfigDescriptor = defaultConfigDescriptor{configCustomConfig = mkCustomConfig properties}
    }

properties :: Properties '[ 'PropertyKey "path" 'TString]
properties =
    emptyProperties
        & defineStringProperty
            #path
            "Set path to 'cabal-fmt' executable"
            "cabal-fmt"

-- | Formatter provider of cabal fmt.
-- Formats the given source in either a given Range or the whole Document.
-- If the provider fails an error is returned that can be displayed to the user.
provider :: Recorder (WithPriority Log) -> PluginId -> FormattingHandler IdeState
provider recorder _ _ _ (FormatRange _) _ _ _ = do
  logWith recorder Info LogInvalidInvocationInfo
  throwError $ PluginInvalidParams "You cannot format a text-range using cabal-fmt."
provider recorder plId ideState _ FormatText contents nuri opts | Just nfp <- uriToNormalizedFilePath nuri = do
  let cabalFmtArgs = [ "--indent", show tabularSize]
      fp = fromNormalizedFilePath nfp
  cabalFmtExePath <- fmap T.unpack $ liftIO $ runAction "cabal-fmt" ideState $ usePropertyAction #path plId properties
  x <- liftIO $ findExecutable cabalFmtExePath
  case x of
    Just _ -> do
      (exitCode, out, err) <-
        liftIO $ Process.readCreateProcessWithExitCode
          ( proc cabalFmtExePath cabalFmtArgs
          )
            { cwd = Just $ takeDirectory fp
            }
          contents
      log Debug $ LogReadCreateProcessInfo err cabalFmtArgs
      case exitCode of
        ExitFailure code -> do
          log Error $ LogProcessInvocationFailure code
          throwError (PluginInternalError "Failed to invoke cabal-fmt")
        ExitSuccess -> do
          let fmtDiff = makeDiffTextEdit contents out
          pure $ InL fmtDiff
    Nothing -> do
      log Error $ LogFormatterBinNotFound cabalFmtExePath
      throwError (PluginInternalError "No installation of cabal-fmt could be found. Please install it globally, or provide the full path to the executable")
  where
    tabularSize = opts ^. L.tabSize
    log = logWith recorder
provider _ _ _ _ _ _ nuri _ = throwError $ PluginInternalError $ "Cabal fmt can only be invoked on files, but uri " <> getUri (fromNormalizedUri nuri) <> " was not a file URI"

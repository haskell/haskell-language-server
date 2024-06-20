{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.CabalGild where

import           Control.Monad.Except             (throwError)
import           Control.Monad.IO.Class
import qualified Data.Text                        as T
import           Development.IDE                  hiding (pluginHandlers)
import           Development.IDE.Core.PluginUtils (mkFormattingHandlers)
import           Ide.Plugin.Error                 (PluginError (PluginInternalError, PluginInvalidParams))
import           Ide.Plugin.Properties
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Protocol.Types
import           Prelude                          hiding (log)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process.ListLike
import qualified System.Process.Text              as Process

data Log
  = LogProcessInvocationFailure Int T.Text
  | LogReadCreateProcessInfo [String]
  | LogInvalidInvocationInfo
  | LogFormatterBinNotFound FilePath
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogProcessInvocationFailure exitCode err ->
      vcat
        [ "Invocation of cabal-gild failed with code" <+> pretty exitCode
        , "Stderr:" <+> pretty err
        ]
    LogReadCreateProcessInfo args ->
      "Formatter invocation: cabal-gild " <+> pretty args
    LogInvalidInvocationInfo -> "Invocation of cabal-gild with range was called but is not supported."
    LogFormatterBinNotFound fp -> "Couldn't find formatter executable 'cabal-gild' at:" <+> pretty fp

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultCabalPluginDescriptor plId "Provides formatting of cabal files with cabal-gild")
    { pluginHandlers = mkFormattingHandlers (provider recorder plId)
    , pluginConfigDescriptor = defaultConfigDescriptor{configCustomConfig = mkCustomConfig properties}
    }

properties :: Properties '[ 'PropertyKey "path" 'TString]
properties =
    emptyProperties
        & defineStringProperty
            #path
            "Set path to 'cabal-gild' executable"
            "cabal-gild"

-- | Formatter provider of cabal gild.
-- Formats the given source in either a given Range or the whole Document.
-- If the provider fails an error is returned that can be displayed to the user.
provider :: Recorder (WithPriority Log) -> PluginId -> FormattingHandler IdeState
provider recorder _ _ _ (FormatRange _) _ _ _ = do
  logWith recorder Info LogInvalidInvocationInfo
  throwError $ PluginInvalidParams "You cannot format a text-range using cabal-gild."
provider recorder plId ideState _ FormatText contents nfp _ = do
  let cabalGildArgs = ["--stdin=" <> fp, "--input=-"] -- < Read from stdin

  cabalGildExePath <- fmap T.unpack $ liftIO $ runAction "cabal-gild" ideState $ usePropertyAction #path plId properties
  x <- liftIO $ findExecutable cabalGildExePath
  case x of
    Just _ -> do
      log Debug $ LogReadCreateProcessInfo cabalGildArgs
      (exitCode, out, err) <-
        liftIO $ Process.readCreateProcessWithExitCode
          ( proc cabalGildExePath cabalGildArgs
          )
            { cwd = Just $ takeDirectory fp
            }
          contents
      case exitCode of
        ExitFailure code -> do
          log Error $ LogProcessInvocationFailure code err
          throwError (PluginInternalError "Failed to invoke cabal-gild")
        ExitSuccess -> do
          let fmtDiff = makeDiffTextEdit contents out
          pure $ InL fmtDiff
    Nothing -> do
      log Error $ LogFormatterBinNotFound cabalGildExePath
      throwError (PluginInternalError "No installation of cabal-gild could be found. Please install it globally, or provide the full path to the executable.")
  where
    fp = fromNormalizedFilePath nfp
    log = logWith recorder

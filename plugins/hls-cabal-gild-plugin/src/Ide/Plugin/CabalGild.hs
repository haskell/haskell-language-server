{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.CabalGild where

import           Control.Monad.Except        (throwError)
import           Control.Monad.IO.Class
import qualified Data.Text                   as T
import           Development.IDE             hiding (pluginHandlers)
import           Ide.Plugin.Error            (PluginError (PluginInternalError, PluginInvalidParams))
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Protocol.Types
import           Prelude                     hiding (log)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process.ListLike
import qualified System.Process.Text         as Process

data Log
  = LogProcessInvocationFailure Int T.Text
  | LogReadCreateProcessInfo [String]
  | LogInvalidInvocationInfo
  | LogFormatterBinNotFound
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
    LogFormatterBinNotFound -> "Couldn't find executable 'cabal-gild'"

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultCabalPluginDescriptor plId "Provides formatting of cabal files with cabal-gild")
    { pluginHandlers = mkFormattingHandlers (provider recorder)
    }

-- | Formatter provider of cabal gild.
-- Formats the given source in either a given Range or the whole Document.
-- If the provider fails an error is returned that can be displayed to the user.
provider :: Recorder (WithPriority Log) -> FormattingHandler IdeState
provider recorder _ _ (FormatRange _) _ _ _ = do
  logWith recorder Info LogInvalidInvocationInfo
  throwError $ PluginInvalidParams "You cannot format a text-range using cabal-gild."
provider recorder _ide _ FormatText contents nfp _ = do
  let cabalGildArgs = ["--stdin=" <> fp, "--input=-"] -- < Read from stdin
  x <- liftIO $ findExecutable "cabal-gild"
  case x of
    Just _ -> do
      log Debug $ LogReadCreateProcessInfo cabalGildArgs
      (exitCode, out, err) <-
        liftIO $ Process.readCreateProcessWithExitCode
          ( proc "cabal-gild" cabalGildArgs
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
      log Error LogFormatterBinNotFound
      throwError (PluginInternalError "No installation of cabal-gild could be found. Please install it into your global environment.")
  where
    fp = fromNormalizedFilePath nfp
    log = logWith recorder

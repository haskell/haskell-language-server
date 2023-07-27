{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.CabalFmt where

import           Control.Lens
import           Control.Monad.Except        (throwError)
import           Control.Monad.IO.Class
import qualified Data.Text                   as T
import           Development.IDE             hiding (pluginHandlers)
import           Ide.Plugin.Error            (PluginError (PluginInternalError, PluginInvalidParams))
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Protocol.Lens  as L
import           Language.LSP.Protocol.Types
import           Prelude                     hiding (log)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process

data Log
  = LogProcessInvocationFailure Int
  | LogReadCreateProcessInfo String [String]
  | LogInvalidInvocationInfo
  | LogCabalFmtNotFound
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogProcessInvocationFailure code -> "Invocation of cabal-fmt failed with code" <+> pretty code
    LogReadCreateProcessInfo stdErrorOut args ->
      vcat $
        ["Invocation of cabal-fmt with arguments" <+> pretty args]
          ++ ["failed with standard error:" <+> pretty stdErrorOut | not (null stdErrorOut)]
    LogInvalidInvocationInfo -> "Invocation of cabal-fmt with range was called but is not supported."
    LogCabalFmtNotFound -> "Couldn't find executable 'cabal-fmt'"

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultCabalPluginDescriptor plId)
    { pluginHandlers = mkFormattingHandlers (provider recorder)
    }

-- | Formatter provider of cabal fmt.
-- Formats the given source in either a given Range or the whole Document.
-- If the provider fails an error is returned that can be displayed to the user.
provider :: Recorder (WithPriority Log) -> FormattingHandler IdeState
provider recorder _ (FormatRange _) _ _ _ = do
  logWith recorder Info LogInvalidInvocationInfo
  throwError $ PluginInvalidParams "You cannot format a text-range using cabal-fmt."
provider recorder _ide FormatText contents nfp opts = do
  let cabalFmtArgs = [fp, "--indent", show tabularSize]
  x <- liftIO $ findExecutable "cabal-fmt"
  case x of
    Just _ -> do
      (exitCode, out, err) <-
        liftIO $ readCreateProcessWithExitCode
          ( proc "cabal-fmt" cabalFmtArgs
          )
            { cwd = Just $ takeDirectory fp
            }
          ""
      log Debug $ LogReadCreateProcessInfo err cabalFmtArgs
      case exitCode of
        ExitFailure code -> do
          log Error $ LogProcessInvocationFailure code
          throwError (PluginInternalError "Failed to invoke cabal-fmt")
        ExitSuccess -> do
          let fmtDiff = makeDiffTextEdit contents (T.pack out)
          pure $ InL fmtDiff
    Nothing -> do
      log Error LogCabalFmtNotFound
      throwError (PluginInternalError "No installation of cabal-fmt could be found. Please install it into your global environment.")
  where
    fp = fromNormalizedFilePath nfp
    tabularSize = opts ^. L.tabSize
    log = logWith recorder

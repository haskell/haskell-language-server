{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.CabalFmt where

import           Control.Lens
import           Control.Monad.IO.Class
import qualified Data.Text               as T
import           Development.IDE         hiding (pluginHandlers)
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Types      as J
import qualified Language.LSP.Types.Lens as J
import           Prelude                 hiding (log)
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
  pure $ Left (ResponseError InvalidRequest "You cannot format a text-range using cabal-fmt." Nothing)
provider recorder _ide FormatText contents nfp opts = liftIO $ do
  let cabalFmtArgs = [fp, "--indent", show tabularSize]
  x <- findExecutable "cabal-fmt"
  case x of
    Just _ -> do
      (exitCode, out, err) <-
        readCreateProcessWithExitCode
          ( proc "cabal-fmt" cabalFmtArgs
          )
            { cwd = Just $ takeDirectory fp
            }
          ""
      log Debug $ LogReadCreateProcessInfo err cabalFmtArgs
      case exitCode of
        ExitFailure code -> do
          log Error $ LogProcessInvocationFailure code
          pure $ Left (ResponseError UnknownErrorCode "Failed to invoke cabal-fmt" Nothing)
        ExitSuccess -> do
          let fmtDiff = makeDiffTextEdit contents (T.pack out)
          pure $ Right fmtDiff
    Nothing -> do
      log Error LogCabalFmtNotFound
      pure $ Left (ResponseError InvalidRequest "No installation of cabal-fmt could be found. Please install it into your global environment." Nothing)
  where
    fp = fromNormalizedFilePath nfp
    tabularSize = opts ^. J.tabSize
    log = logWith recorder

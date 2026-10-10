{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Ide.Plugin.CabalProject.Rules
  ( projectRules
  , ParseProjectFields (..)
  , kick
  , Log (..)
  ) where

import           Control.DeepSeq                     (NFData)
import           Control.Monad.IO.Class
import qualified Data.ByteString                     as BS
import           Data.Hashable                       (Hashable)
import qualified Data.HashMap.Strict                 as HashMap
import           Data.Proxy                          (Proxy (..))
import qualified Data.Text                           as T
import           Data.Text.Encoding                  (encodeUtf8)
import           Data.Text.Utf16.Rope.Mixed          as Rope
import           Development.IDE
import qualified Development.IDE.Core.Shake          as Shake
import qualified Distribution.Fields                 as Syntax
import qualified Distribution.Parsec                 as Syntax
import           GHC.Generics                        (Generic)
import           Ide.Logger
import           Ide.Plugin.Cabal.Orphans            ()
import qualified Ide.Plugin.CabalProject.Diagnostics as Diagnostics
import qualified Ide.Plugin.CabalProject.OfInterest  as OfInterest
import           Ide.Types

data ParseProjectFields = ParseProjectFields
  deriving (Eq, Show, Generic)

instance Hashable ParseProjectFields

instance NFData ParseProjectFields

type instance RuleResult ParseProjectFields = [Syntax.Field Syntax.Position]

data Log
  = LogModificationTime NormalizedFilePath FileVersion
  | LogShake Shake.Log
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogShake log' -> pretty log'
    LogModificationTime nfp modTime ->
      "Modified:" <+> pretty (fromNormalizedFilePath nfp) <+> pretty (show modTime)

projectRules :: Recorder (WithPriority Log) -> PluginId -> Rules ()
projectRules recorder plId = do
  -- Make sure we initialise the project files-of-interest.
  OfInterest.ofInterestRules
  -- Rule to produce diagnostics for project files.
  define (cmapWithPrio LogShake recorder) $ \ParseProjectFields file -> do
    config <- getPluginConfigAction plId
    if not (plcGlobalOn config && plcDiagnosticsOn config)
      then pure ([], Nothing)
      else do
        -- whenever this key is marked as dirty (e.g., when a user writes stuff to it),
        -- we rerun this rule because this rule *depends* on GetModificationTime.
        (t, mProjectSource) <- use_ GetFileContents file
        log' Debug $ LogModificationTime file t
        contents <- case mProjectSource of
          Just source -> pure $ encodeUtf8 $ Rope.toText source
          Nothing     -> liftIO $ BS.readFile $ fromNormalizedFilePath file

        case Syntax.readFields' contents of
          Left err ->
            pure
              ( [ Diagnostics.fatalParseErrorDiagnostic file $
                    "Failed to parse project file: " <> T.pack (show err)
              ]
              , Nothing
              )
          Right (fields, _lexerWarnings) -> do
            -- lexer warnings are not rendered; the cabal plugin ignores them as well
            let schemaDiags = Diagnostics.checkProjectFields file fields
            pure (schemaDiags, Just fields)

  action $ do
    -- Run the project kick. This code always runs when 'shakeRestart' is run.
    -- Must be careful to not impede the performance too much. Crucial to
    -- a snappy IDE experience.
    kick
  where
    log' = logWith recorder

{- | This is the kick function for the cabal-project plugin.
We run this action, whenever the shake session is run/restarted, which triggers
actions to produce diagnostics for project files.

It is paramount that this kick-function can be run quickly, since it is a
blocking function invocation.
-}
kick :: Action ()
kick = do
  files <- HashMap.keys <$> OfInterest.getProjectFilesOfInterestUntracked
  Shake.runWithSignal (Proxy @"kick/start/cabal-project") (Proxy @"kick/done/cabal-project") files ParseProjectFields

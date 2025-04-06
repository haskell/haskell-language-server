{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Ide.Plugin.CabalProject (descriptor, Log (..)) where

import           Control.DeepSeq
import           Control.Lens                                  ((^.))
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import qualified Data.ByteString                               as BS
import qualified Data.Text                                     as T
import qualified Data.Text.Encoding                            as Encoding
import           Data.Text.Utf16.Rope.Mixed                    as Rope
import           Development.IDE                               as D
import           Development.IDE.Core.PluginUtils
import qualified Development.IDE.Core.Shake                    as Shake
import           Development.IDE.Graph                         (Key, alwaysRerun)
import           Ide.Types
import qualified Language.LSP.Protocol.Lens                    as JL
import qualified Language.LSP.Protocol.Message                 as LSP
import           Language.LSP.Protocol.Types
import qualified Language.LSP.VFS                              as VFS

data Log
  = LogModificationTime NormalizedFilePath FileVersion
  | LogShake Shake.Log
  | LogDocOpened Uri
  | LogDocModified Uri
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

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId "Cabal Project File Support")
  { pluginHandlers = mkPluginHandler LSP.SMethod_TextDocumentDidOpen $
      \ideState _ vfs (DidOpenTextDocumentParams TextDocumentItem{_uri,_version,_text}) -> do
        let filePath = toNormalizedUri _uri
        logWith recorder Debug $ LogDocOpened _uri
        when (isCabalProjectFile filePath) $ do
          -- TODO: Initialize project file handling
          pure ()
  , pluginHandlers = mkPluginHandler LSP.SMethod_TextDocumentDidChange $
      \ideState _ vfs (DidChangeTextDocumentParams TextDocumentIdentifier{_uri} _ _) -> do
        let filePath = toNormalizedUri _uri
        logWith recorder Debug $ LogDocModified _uri
        when (isCabalProjectFile filePath) $ do
          -- TODO: Handle file changes
          pure ()
  , pluginRules = cabalProjectRules recorder plId
  }

isCabalProjectFile :: NormalizedFilePath -> Bool
isCabalProjectFile filePath = "cabal.project" `T.isSuffixOf` T.pack (fromNormalizedFilePath filePath)

cabalProjectRules :: Recorder (WithPriority Log) -> PluginId -> Rules ()
cabalProjectRules recorder plId = do
  -- TODO: Add rules for parsing and validating cabal.project files
  pure () 
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Ide.Plugin.Config
    (
      getInitialConfig
    , getConfigFromNotification
    , Config(..)
    , PluginConfig(..)
    , CheckParents(..)
    ) where

import           Control.Applicative
import qualified Data.Aeson                    as A
import           Data.Aeson              hiding ( Error )
import           Data.Default
import qualified Data.Text                     as T
import           Language.Haskell.LSP.Types
import qualified Data.Map as Map
import GHC.Generics (Generic)

-- ---------------------------------------------------------------------

-- | Given a DidChangeConfigurationNotification message, this function returns the parsed
-- Config object if possible.
getConfigFromNotification :: DidChangeConfigurationNotification -> Either T.Text Config
getConfigFromNotification (NotificationMessage _ _ (DidChangeConfigurationParams p)) =
  case fromJSON p of
    A.Success c -> Right c
    A.Error err -> Left $ T.pack err

-- | Given an InitializeRequest message, this function returns the parsed
-- Config object if possible. Otherwise, it returns the default configuration
getInitialConfig :: InitializeRequest -> Either T.Text Config
getInitialConfig (RequestMessage _ _ _ InitializeParams{_initializationOptions = Nothing }) = Right def
getInitialConfig (RequestMessage _ _ _ InitializeParams{_initializationOptions = Just opts}) =
  case fromJSON opts of
    A.Success c -> Right c
    A.Error err -> Left $ T.pack err

-- ---------------------------------------------------------------------
data CheckParents
    -- Note that ordering of constructors is meaningful and must be monotonically
    -- increasing in the scenarios where parents are checked
    = NeverCheck
    | CheckOnClose
    | CheckOnSaveAndClose
    | AlwaysCheck
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- | We (initially anyway) mirror the hie configuration, so that existing
-- clients can simply switch executable and not have any nasty surprises.  There
-- will be surprises relating to config options being ignored, initially though.
data Config =
  Config
    { checkParents                :: CheckParents
    , checkProject                :: !Bool
    , hlintOn                     :: !Bool
    , diagnosticsOnChange         :: !Bool
    , maxNumberOfProblems         :: !Int
    , diagnosticsDebounceDuration :: !Int
    , liquidOn                    :: !Bool
    , completionSnippetsOn        :: !Bool
    , formatOnImportOn            :: !Bool
    , formattingProvider          :: !T.Text
    , plugins                     :: !(Map.Map T.Text PluginConfig)
    } deriving (Show,Eq)

instance Default Config where
  def = Config
    { checkParents                = CheckOnSaveAndClose
    , checkProject                = True
    , hlintOn                     = True
    , diagnosticsOnChange         = True
    , maxNumberOfProblems         = 100
    , diagnosticsDebounceDuration = 350000
    , liquidOn                    = False
    , completionSnippetsOn        = True
    , formatOnImportOn            = True
    -- , formattingProvider          = "brittany"
    , formattingProvider          = "ormolu"
    -- , formattingProvider          = "floskell"
    -- , formattingProvider          = "stylish-haskell"
    , plugins                     = Map.empty
    }

-- TODO: Add API for plugins to expose their own LSP config options
instance A.FromJSON Config where
  parseJSON = A.withObject "Config" $ \v -> do
    -- Officially, we use "haskell" as the section name but for
    -- backwards compatibility we also accept "languageServerHaskell"
    s <- v .: "haskell" <|> v .: "languageServerHaskell"
    flip (A.withObject "Config.settings") s $ \o -> Config
      <$> (o .:? "checkParents" <|> v .:? "checkParents") .!= checkParents def
      <*> (o .:? "checkProject" <|> v .:? "checkProject") .!= checkProject def
      <*> o .:? "hlintOn"                                 .!= hlintOn def
      <*> o .:? "diagnosticsOnChange"                     .!= diagnosticsOnChange def
      <*> o .:? "maxNumberOfProblems"                     .!= maxNumberOfProblems def
      <*> o .:? "diagnosticsDebounceDuration"             .!= diagnosticsDebounceDuration def
      <*> o .:? "liquidOn"                                .!= liquidOn def
      <*> o .:? "completionSnippetsOn"                    .!= completionSnippetsOn def
      <*> o .:? "formatOnImportOn"                        .!= formatOnImportOn def
      <*> o .:? "formattingProvider"                      .!= formattingProvider def
      <*> o .:? "plugin"                                  .!= plugins def

-- 2017-10-09 23:22:00.710515298 [ThreadId 11] - ---> {"jsonrpc":"2.0","method":"workspace/didChangeConfiguration","params":{"settings":{"haskell":{"maxNumberOfProblems":100,"hlintOn":true}}}}
-- 2017-10-09 23:22:00.710667381 [ThreadId 15] - reactor:got didChangeConfiguration notification:
-- NotificationMessage
--   {_jsonrpc = "2.0"
--   , _method = WorkspaceDidChangeConfiguration
--   , _params = DidChangeConfigurationParams
--                 {_settings = Object (fromList [("haskell",Object (fromList [("hlintOn",Bool True)
--                                                                            ,("maxNumberOfProblems",Number 100.0)]))])}}

instance A.ToJSON Config where
  toJSON Config{..} =
      object [ "haskell" .= r ]
    where
      r = object [ "checkParents"                .= checkParents
                 , "checkProject"                .= checkProject
                 , "hlintOn"                     .= hlintOn
                 , "diagnosticsOnChange"         .= diagnosticsOnChange
                 , "maxNumberOfProblems"         .= maxNumberOfProblems
                 , "diagnosticsDebounceDuration" .= diagnosticsDebounceDuration
                 , "liquidOn"                    .= liquidOn
                 , "completionSnippetsOn"        .= completionSnippetsOn
                 , "formatOnImportOn"            .= formatOnImportOn
                 , "formattingProvider"          .= formattingProvider
                 , "plugin"                      .= plugins
                 ]

-- ---------------------------------------------------------------------

-- | A PluginConfig is a generic configuration for a given HLS plugin.  It
-- provides a "big switch" to turn it on or off as a whole, as well as small
-- switches per feature, and a slot for custom config.
-- This provides a regular naming scheme for all plugin config.
data PluginConfig =
    PluginConfig
      { plcGlobalOn      :: !Bool
      , plcCodeActionsOn :: !Bool
      , plcCodeLensOn    :: !Bool
      , plcDiagnosticsOn :: !Bool
      , plcHoverOn       :: !Bool
      , plcSymbolsOn     :: !Bool
      , plcCompletionOn  :: !Bool
      , plcRenameOn      :: !Bool
      , plcConfig        :: !A.Object
      } deriving (Show,Eq)

instance Default PluginConfig where
  def = PluginConfig
      { plcGlobalOn      = True
      , plcCodeActionsOn = True
      , plcCodeLensOn    = True
      , plcDiagnosticsOn = True
      , plcHoverOn       = True
      , plcSymbolsOn     = True
      , plcCompletionOn  = True
      , plcRenameOn      = True
      , plcConfig        = mempty
      }

instance A.ToJSON PluginConfig where
    toJSON (PluginConfig g ca cl d h s c rn cfg) = r
      where
        r = object [ "globalOn"      .= g
                   , "codeActionsOn" .= ca
                   , "codeLensOn"    .= cl
                   , "diagnosticsOn" .= d
                   , "hoverOn"       .= h
                   , "symbolsOn"     .= s
                   , "completionOn"  .= c
                   , "renameOn"      .= rn
                   , "config"        .= cfg
                   ]

instance A.FromJSON PluginConfig where
  parseJSON = A.withObject "PluginConfig" $ \o  -> PluginConfig
      <$> o .:? "globalOn"      .!= plcGlobalOn def
      <*> o .:? "codeActionsOn" .!= plcCodeActionsOn def
      <*> o .:? "codeLensOn"    .!= plcCodeLensOn    def
      <*> o .:? "diagnosticsOn" .!= plcDiagnosticsOn def -- AZ
      <*> o .:? "hoverOn"       .!= plcHoverOn       def
      <*> o .:? "symbolsOn"     .!= plcSymbolsOn     def
      <*> o .:? "completionOn"  .!= plcCompletionOn  def
      <*> o .:? "renameOn"      .!= plcRenameOn      def
      <*> o .:? "config"        .!= plcConfig        def

-- ---------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
module Ide.Plugin.Config
    ( getConfigFromNotification
    , Config(..)
    , parseConfig
    , PluginConfig(..)
    , CheckParents(..)
    ) where

import           Control.Applicative
import           Data.Aeson          hiding (Error)
import qualified Data.Aeson          as A
import qualified Data.Aeson.Types    as A
import           Data.Default
import qualified Data.Map            as Map
import qualified Data.Text           as T
import           GHC.Generics        (Generic)

-- ---------------------------------------------------------------------

-- | Given a DidChangeConfigurationNotification message, this function returns the parsed
-- Config object if possible.
getConfigFromNotification :: Config -> A.Value -> Either T.Text Config
getConfigFromNotification defaultValue p =
  case A.parse (parseConfig defaultValue) p of
    A.Success c -> Right c
    A.Error err -> Left $ T.pack err

-- ---------------------------------------------------------------------
data CheckParents
    -- Note that ordering of constructors is meaningful and must be monotonically
    -- increasing in the scenarios where parents are checked
    = NeverCheck
    | CheckOnSave
    | AlwaysCheck
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | We (initially anyway) mirror the hie configuration, so that existing
-- clients can simply switch executable and not have any nasty surprises.  There
-- will be surprises relating to config options being ignored, initially though.
data Config =
  Config
    { checkParents            :: CheckParents
    , checkProject            :: !Bool
    , formattingProvider      :: !T.Text
    , cabalFormattingProvider :: !T.Text
    , maxCompletions          :: !Int
    , plugins                 :: !(Map.Map T.Text PluginConfig)
    } deriving (Show,Eq)

instance Default Config where
  def = Config
    { checkParents                = CheckOnSave
    , checkProject                = True
    -- , formattingProvider          = "brittany"
    , formattingProvider          = "ormolu"
    -- , formattingProvider          = "floskell"
    -- , formattingProvider          = "stylish-haskell"
    , cabalFormattingProvider     = "cabal-fmt"
    , maxCompletions              = 40
    , plugins                     = Map.empty
    }

-- TODO: Add API for plugins to expose their own LSP config options
parseConfig :: Config -> Value -> A.Parser Config
parseConfig defValue = A.withObject "Config" $ \v -> do
    -- Officially, we use "haskell" as the section name but for
    -- backwards compatibility we also accept "languageServerHaskell"
    c <- v .: "haskell" <|> v .:? "languageServerHaskell"
    case c of
      Nothing -> return defValue
      Just s -> flip (A.withObject "Config.settings") s $ \o -> Config
        <$> (o .:? "checkParents" <|> v .:? "checkParents") .!= checkParents defValue
        <*> (o .:? "checkProject" <|> v .:? "checkProject") .!= checkProject defValue
        <*> o .:? "formattingProvider"                      .!= formattingProvider defValue
        <*> o .:? "cabalFormattingProvider"                 .!= cabalFormattingProvider defValue
        <*> o .:? "maxCompletions"                          .!= maxCompletions defValue
        <*> o .:? "plugin"                                  .!= plugins defValue

instance A.ToJSON Config where
  toJSON Config{..} =
      object [ "haskell" .= r ]
    where
      r = object [ "checkParents"                .= checkParents
                 , "checkProject"                .= checkProject
                 , "formattingProvider"          .= formattingProvider
                 , "maxCompletions"              .= maxCompletions
                 , "plugin"                      .= plugins
                 ]

-- ---------------------------------------------------------------------

-- | A PluginConfig is a generic configuration for a given HLS plugin.  It
-- provides a "big switch" to turn it on or off as a whole, as well as small
-- switches per feature, and a slot for custom config.
-- This provides a regular naming scheme for all plugin config.
data PluginConfig =
    PluginConfig
      { plcGlobalOn         :: !Bool
      , plcCallHierarchyOn  :: !Bool
      , plcCodeActionsOn    :: !Bool
      , plcCodeLensOn       :: !Bool
      , plcDiagnosticsOn    :: !Bool
      , plcHoverOn          :: !Bool
      , plcSymbolsOn        :: !Bool
      , plcCompletionOn     :: !Bool
      , plcRenameOn         :: !Bool
      , plcSelectionRangeOn :: !Bool
      , plcFoldingRangeOn   :: !Bool
      , plcConfig           :: !A.Object
      } deriving (Show,Eq)

instance Default PluginConfig where
  def = PluginConfig
      { plcGlobalOn         = True
      , plcCallHierarchyOn  = True
      , plcCodeActionsOn    = True
      , plcCodeLensOn       = True
      , plcDiagnosticsOn    = True
      , plcHoverOn          = True
      , plcSymbolsOn        = True
      , plcCompletionOn     = True
      , plcRenameOn         = True
      , plcSelectionRangeOn = True
      , plcFoldingRangeOn = True
      , plcConfig           = mempty
      }

instance A.ToJSON PluginConfig where
    toJSON (PluginConfig g ch ca cl d h s c rn sr fr cfg) = r
      where
        r = object [ "globalOn"         .= g
                   , "callHierarchyOn"  .= ch
                   , "codeActionsOn"    .= ca
                   , "codeLensOn"       .= cl
                   , "diagnosticsOn"    .= d
                   , "hoverOn"          .= h
                   , "symbolsOn"        .= s
                   , "completionOn"     .= c
                   , "renameOn"         .= rn
                   , "selectionRangeOn" .= sr
                   , "foldingRangeOn"   .= fr
                   , "config"           .= cfg
                   ]

instance A.FromJSON PluginConfig where
  parseJSON = A.withObject "PluginConfig" $ \o  -> PluginConfig
      <$> o .:? "globalOn"         .!= plcGlobalOn def
      <*> o .:? "callHierarchyOn"  .!= plcCallHierarchyOn def
      <*> o .:? "codeActionsOn"    .!= plcCodeActionsOn def
      <*> o .:? "codeLensOn"       .!= plcCodeLensOn    def
      <*> o .:? "diagnosticsOn"    .!= plcDiagnosticsOn def -- AZ
      <*> o .:? "hoverOn"          .!= plcHoverOn       def
      <*> o .:? "symbolsOn"        .!= plcSymbolsOn     def
      <*> o .:? "completionOn"     .!= plcCompletionOn  def
      <*> o .:? "renameOn"         .!= plcRenameOn      def
      <*> o .:? "selectionRangeOn" .!= plcSelectionRangeOn def
      <*> o .:? "foldingRangeOn" .!= plcFoldingRangeOn def
      <*> o .:? "config"           .!= plcConfig        def

-- ---------------------------------------------------------------------

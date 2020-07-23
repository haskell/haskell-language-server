{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Ide.Plugin.Config
    (
      getInitialConfig
    , getConfigFromNotification
    , Config(..)
    ) where

import qualified Data.Aeson                    as A
import           Data.Aeson              hiding ( Error )
import           Data.Default
import qualified Data.Text                     as T
import           Language.Haskell.LSP.Types

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

-- | We (initially anyway) mirror the hie configuration, so that existing
-- clients can simply switch executable and not have any nasty surprises.  There
-- will be surprises relating to config options being ignored, initially though.
data Config =
  Config
    { hlintOn                     :: Bool
    , diagnosticsOnChange         :: Bool
    , maxNumberOfProblems         :: Int
    , diagnosticsDebounceDuration :: Int
    , liquidOn                    :: Bool
    , completionSnippetsOn        :: Bool
    , formatOnImportOn            :: Bool
    , formattingProvider          :: T.Text
    } deriving (Show,Eq)

instance Default Config where
  def = Config
    { hlintOn                     = True
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
    }

-- TODO: Add API for plugins to expose their own LSP config options
instance A.FromJSON Config where
  parseJSON = A.withObject "Config" $ \v -> do
    s <- v .: "haskell"
    flip (A.withObject "Config.settings") s $ \o -> Config
      <$> o .:? "hlintOn"                     .!= hlintOn def
      <*> o .:? "diagnosticsOnChange"         .!= diagnosticsOnChange def
      <*> o .:? "maxNumberOfProblems"         .!= maxNumberOfProblems def
      <*> o .:? "diagnosticsDebounceDuration" .!= diagnosticsDebounceDuration def
      <*> o .:? "liquidOn"                    .!= liquidOn def
      <*> o .:? "completionSnippetsOn"        .!= completionSnippetsOn def
      <*> o .:? "formatOnImportOn"            .!= formatOnImportOn def
      <*> o .:? "formattingProvider"          .!= formattingProvider def

-- 2017-10-09 23:22:00.710515298 [ThreadId 11] - ---> {"jsonrpc":"2.0","method":"workspace/didChangeConfiguration","params":{"settings":{"haskell":{"maxNumberOfProblems":100,"hlintOn":true}}}}
-- 2017-10-09 23:22:00.710667381 [ThreadId 15] - reactor:got didChangeConfiguration notification:
-- NotificationMessage
--   {_jsonrpc = "2.0"
--   , _method = WorkspaceDidChangeConfiguration
--   , _params = DidChangeConfigurationParams
--                 {_settings = Object (fromList [("haskell",Object (fromList [("hlintOn",Bool True)
--                                                                            ,("maxNumberOfProblems",Number 100.0)]))])}}

instance A.ToJSON Config where
  toJSON (Config h diag m d l c f fp) = object [ "haskell" .= r ]
    where
      r = object [ "hlintOn"                     .= h
                 , "diagnosticsOnChange"         .= diag
                 , "maxNumberOfProblems"         .= m
                 , "diagnosticsDebounceDuration" .= d
                 , "liquidOn"                    .= l
                 , "completionSnippetsOn"        .= c
                 , "formatOnImportOn"            .= f
                 , "formattingProvider"          .= fp
                 ]

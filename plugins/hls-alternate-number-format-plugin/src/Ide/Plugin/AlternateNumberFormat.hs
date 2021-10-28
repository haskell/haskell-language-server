{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards #-}
module Ide.Plugin.AlternateNumberFormat where

import           Compat.HieTypes              (HieASTs (HieASTs))
import           Control.Lens                 ((^.))
import           Control.Monad.Except         (ExceptT, MonadIO, forM_, liftIO,
                                               runExceptT, throwError)
import           Data.Aeson                   (FromJSON, ToJSON, Value (Null))
import qualified Data.Map                     as M
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Development.IDE              (GetHieAst(GetHieAst), HieAstResult (HAR, hieAst),
                                               IdeState, ideLogger, runAction,
                                               use)
import           Development.IDE.GHC.Compat   (HieASTs (getAsts),
                                               )
import           Development.IDE.Types.Logger as Logger
import           Ide.Plugin.Retrie            (handleMaybe, handleMaybeM,
                                               response)
import           Ide.Types
import           Language.LSP.Types
import           Language.LSP.Types.Lens      (textDocument, uri)

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentHover codeActionHandler
    , pluginCommands = commands
    }

commands :: [PluginCommand IdeState]
commands = [PluginCommand "alternateNumberFormat" "Provide alternate number formats based on LanguagePragmas and Type" provideFormat]

newtype AlternateNumberParams = AlternateNumberParams Int
    deriving(FromJSON, ToJSON)

provideFormat :: CommandFunction IdeState AlternateNumberParams
provideFormat _ _ = pure $ Right Null

codeActionHandler :: PluginMethodHandler IdeState TextDocumentHover
codeActionHandler state plId caParams = response $ do
    liftIO $ Logger.logError (ideLogger state) "We are HERE"
    nfp <- getNormalizedFilePath caParams
    liftIO $ Logger.logError (ideLogger state) (T.pack $ show nfp)
    HAR{hieAst} <- getHieAst state nfp
    liftIO $ Logger.logError (ideLogger state) "PRE-KEYS"
    -- liftIO $ forM_ (M.keys (getAsts hieAst)) (\x -> Logger.logError (ideLogger state) (T.pack $ show x))
    liftIO $ Logger.logError (ideLogger state) "POST-KEYS"
    pure Nothing

getNormalizedFilePath :: Monad m => HoverParams -> ExceptT String m NormalizedFilePath
getNormalizedFilePath caParams = handleMaybe "Error: converting to NormalizedFilePath"
        $ uriToNormalizedFilePath
        $ toNormalizedUri (caParams ^. (textDocument . uri))

getHieAst :: MonadIO m => IdeState -> NormalizedFilePath -> ExceptT String m HieAstResult
getHieAst state = handleMaybeM "Error: Could not get AST" . liftIO . runAction "AlternateNumberFormat.GetHieAst" state . use GetHieAst

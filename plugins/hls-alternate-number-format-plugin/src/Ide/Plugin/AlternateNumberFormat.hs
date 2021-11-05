{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Ide.Plugin.AlternateNumberFormat where

import           Control.Lens                         ((^.))
import           Control.Monad.Except                 (ExceptT, MonadIO, liftIO)
import           Data.Aeson                           (FromJSON, ToJSON,
                                                       Value (Null))
import qualified Data.Text                            as T
import           Development.IDE                      (GetParsedModuleWithComments (GetParsedModuleWithComments),
                                                       IdeState,
                                                       TcModuleResult (TcModuleResult, tmrParsed, tmrTypechecked),
                                                       TypeCheck (TypeCheck),
                                                       ideLogger, runAction,
                                                       useWithStale)
import           Development.IDE.Core.PositionMapping (PositionMapping)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util      (FastString, mkFastString)
import           Development.IDE.Types.Logger         as Logger
import           Ide.Plugin.Retrie                    (handleMaybe,
                                                       handleMaybeM, response)
import           Ide.Plugin.Traverse.Literals         (collectLiterals
                                                       )
import           Ide.Types
import           Language.LSP.Types
import           Language.LSP.Types.Lens              (textDocument, uri)
import           Prelude                              hiding (log)

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

codeActionHandler :: PluginMethodHandler IdeState 'TextDocumentHover
codeActionHandler state _ caParams = response $ do
    logIO' state "We are HERE"
    nfp <- getNormalizedFilePath caParams
    logIO state nfp
    (pm, _) <- getHieAst state nfp
    -- (TcModuleResult{..}, _) <- getHieAst state nfp
    logIO state (collectLiterals pm)
    logIO state $ "File: " <> nfpToFastString nfp
    -- logIO state (collectTcLiterals $ tcg_binds tmrTypechecked )
    pure Nothing

getNormalizedFilePath :: Monad m => HoverParams -> ExceptT String m NormalizedFilePath
getNormalizedFilePath caParams = handleMaybe "Error: converting to NormalizedFilePath"
        $ uriToNormalizedFilePath
        $ toNormalizedUri (caParams ^. (textDocument . uri))

getHieAst :: MonadIO m => IdeState -> NormalizedFilePath -> ExceptT String m (ParsedModule, PositionMapping)
getHieAst state = handleMaybeM "Error: Could not get AST" . liftIO . runAction "AlternateNumberFormat.GetHieAst" state . useWithStale GetParsedModuleWithComments

nfpToFastString :: NormalizedFilePath -> FastString
nfpToFastString = mkFastString . fromNormalizedFilePath

logIO :: (MonadIO m, Show a) => IdeState -> a -> m ()
logIO state = liftIO . log state

log :: Show a => IdeState -> a -> IO ()
log state = log' state . show

logIO' :: MonadIO m => IdeState -> String -> m ()
logIO' state = liftIO . log' state

log' :: IdeState -> String -> IO ()
log' state = Logger.logError (ideLogger state) . T.pack


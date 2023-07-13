{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}

-- |Debug utilities
module Ide.Plugin.Eval.Util (
    timed,
    isLiterate,
    response',
    gStrictTry,
    logWith,
) where

import           Control.Exception                     (SomeException, evaluate,
                                                        fromException)
import           Control.Monad.IO.Class                (MonadIO (liftIO))
import           Control.Monad.Trans.Except            (ExceptT (..),
                                                        runExceptT)
import           Data.Aeson                            (Value (Null))
import           Data.String                           (IsString (fromString))
import qualified Data.Text                             as T
import           Development.IDE                       (IdeState, Priority (..),
                                                        ideLogger, logPriority)
import           Development.IDE.Core.PluginUtils      (GhcidePluginError)
import qualified Development.IDE.Core.PluginUtils      as PluginUtils
import           Development.IDE.GHC.Compat.Outputable
import           Development.IDE.GHC.Compat.Util       (MonadCatch, bagToList,
                                                        catch)
import           GHC.Exts                              (toList)
import           GHC.Stack                             (HasCallStack, callStack,
                                                        srcLocFile,
                                                        srcLocStartCol,
                                                        srcLocStartLine)
import           Ide.PluginUtils                       (prettyPluginError)
import           Language.LSP.Server
import           Language.LSP.Types
import           System.FilePath                       (takeExtension)
import           System.Time.Extra                     (duration, showDuration)
import           UnliftIO.Exception                    (catchAny)

timed :: MonadIO m => (t -> String -> m a) -> t -> m b -> m b
timed out name op = do
    (secs, r) <- duration op
    _ <- out name (showDuration secs)
    return r

-- | Log using hie logger, reports source position of logging statement
logWith :: (HasCallStack, MonadIO m, Show a1, Show a2) => IdeState -> a1 -> a2 -> m ()
logWith state key val =
    liftIO . logPriority (ideLogger state) logLevel $
        T.unwords
            [T.pack logWithPos, asT key, asT val]
  where
    logWithPos =
        let stk = toList callStack
            pr pos = concat [srcLocFile pos, ":", show . srcLocStartLine $ pos, ":", show . srcLocStartCol $ pos]
         in if null stk then "" else pr . snd . head $ stk

    asT :: Show a => a -> T.Text
    asT = T.pack . show

-- | Set to Info to see extensive debug info in hie log, set to Debug in production
logLevel :: Priority
logLevel = Debug -- Info

isLiterate :: FilePath -> Bool
isLiterate x = takeExtension x `elem` [".lhs", ".lhs-boot"]

response' :: ExceptT GhcidePluginError (LspM c) WorkspaceEdit -> LspM c (Either ResponseError Value)
response' act = do
    res <- runExceptT act
             `catchAny` \e -> do
                res <- showErr e
                pure . Left . PluginUtils.mkPluginErrorMessage $ fromString res
    case res of
      Left e ->
          return $ Left $ PluginUtils.handlePluginError e
      Right a -> do
        _ <- sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing a) (\_ -> pure ())
        return $ Right Null

gStrictTry :: (MonadIO m, MonadCatch m) => m b -> m (Either String b)
gStrictTry op =
    catch
        (op >>= fmap Right . gevaluate)
        (fmap Left . showErr)

gevaluate :: MonadIO m => a -> m a
gevaluate = liftIO . evaluate

showErr :: Monad m => SomeException -> m String
showErr e =
#if MIN_VERSION_ghc(9,3,0)
  case fromException e of
    -- On GHC 9.4+, the show instance adds the error message span
    -- We don't want this for the plugin
    -- So render without the span.
    Just (SourceError msgs) -> return $ renderWithContext defaultSDocContext
                                      $ vcat
                                      $ bagToList
                                      $ fmap (vcat . unDecorated
                                                   . diagnosticMessage
#if MIN_VERSION_ghc(9,5,0)
                                                    (defaultDiagnosticOpts @GhcMessage)
#endif
                                                   . errMsgDiagnostic)
                                      $ getMessages msgs
    _ ->
#endif
      return . show $ e

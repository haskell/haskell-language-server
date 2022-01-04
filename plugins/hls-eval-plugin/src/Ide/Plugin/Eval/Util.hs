{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |Debug utilities
module Ide.Plugin.Eval.Util (
    asS,
    timed,
    isLiterate,
    response',
    gStrictTry,
    logWith,
) where

import           Control.Exception               (SomeException, evaluate)
import           Control.Monad.IO.Class          (MonadIO (liftIO))
import           Control.Monad.Trans.Except      (ExceptT (..), runExceptT)
import           Data.Aeson                      (Value (Null))
import           Data.String                     (IsString (fromString))
import qualified Data.Text                       as T
import           Development.IDE                 (IdeState, Priority (..),
                                                  ideLogger, logPriority)
import           Development.IDE.GHC.Compat      (Outputable, ppr,
                                                  showSDocUnsafe)
import           Development.IDE.GHC.Compat.Util (MonadCatch, catch)
import           GHC.Exts                        (toList)
import           GHC.Stack                       (HasCallStack, callStack,
                                                  srcLocFile, srcLocStartCol,
                                                  srcLocStartLine)
import           Language.LSP.Server
import           Language.LSP.Types
import           System.FilePath                 (takeExtension)
import           System.Time.Extra               (duration, showDuration)
import           UnliftIO.Exception              (catchAny)

asS :: Outputable a => a -> String
asS = showSDocUnsafe . ppr

timed :: MonadIO m => (t -> String -> m a) -> t -> m b -> m b
timed out name op = do
    (secs, r) <- duration op
    _ <- out name (showDuration secs)
    return r

-- |Log using hie logger, reports source position of logging statement
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

response' :: ExceptT String (LspM c) WorkspaceEdit -> LspM c (Either ResponseError Value)
response' act = do
    res <- runExceptT act
             `catchAny` showErr
    case res of
      Left e ->
          return $ Left (ResponseError InternalError (fromString e) Nothing)
      Right a -> do
        _ <- sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing a) (\_ -> pure ())
        return $ Right Null

gStrictTry :: (MonadIO m, MonadCatch m) => m b -> m (Either String b)
gStrictTry op =
    catch
        (op >>= fmap Right . gevaluate)
        showErr

gevaluate :: MonadIO m => a -> m a
gevaluate = liftIO . evaluate

showErr :: Monad m => SomeException -> m (Either String b)
showErr = return . Left . show

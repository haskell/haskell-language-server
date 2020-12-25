{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |Debug utilities
module Ide.Plugin.Eval.Util (
    asS,
    timed,
    isLiterate,
    handleMaybe,
    handleMaybeM,
    response,
    response',
    gStrictTry,
    logWith,
) where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (
    ExceptT (..),
    runExceptT,
    throwE,
 )
import Data.Aeson (Value (Null))
import Data.Bifunctor (first)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Development.IDE (
    IdeState,
    Priority (..),
    ideLogger,
    logPriority,
 )
import Exception (ExceptionMonad, SomeException (..), evaluate, gcatch)
import GHC.Exts (toList)
import GHC.Stack (HasCallStack, callStack, srcLocFile, srcLocStartCol, srcLocStartLine)
import Language.Haskell.LSP.Types (
    ErrorCode (InternalError),
    ResponseError (ResponseError),
 )
import Outputable (
    Outputable (ppr),
    ppr,
    showSDocUnsafe,
 )
import System.FilePath (takeExtension)
import System.Time.Extra (
    duration,
    showDuration,
 )

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

handleMaybe :: Monad m => e -> Maybe b -> ExceptT e m b
handleMaybe msg = maybe (throwE msg) return

handleMaybeM :: Monad m => e -> m (Maybe b) -> ExceptT e m b
handleMaybeM msg act = maybe (throwE msg) return =<< lift act

response :: Functor f => ExceptT String f c -> f (Either ResponseError c)
response =
    fmap (first (\msg -> ResponseError InternalError (fromString msg) Nothing))
        . runExceptT

response' :: ExceptT String IO a -> IO (Either ResponseError Value, Maybe a)
response' act = do
    res <- gStrictTry $ runExceptT act
    case join res of
        Left e ->
            return
                (Left (ResponseError InternalError (fromString e) Nothing), Nothing)
        Right a -> return (Right Null, Just a)

gStrictTry :: ExceptionMonad m => m b -> m (Either String b)
gStrictTry op =
    gcatch
        (op >>= fmap Right . gevaluate)
        showErr

gevaluate :: MonadIO m => a -> m a
gevaluate = liftIO . evaluate

showErr :: Monad m => SomeException -> m (Either String b)
showErr = return . Left . show

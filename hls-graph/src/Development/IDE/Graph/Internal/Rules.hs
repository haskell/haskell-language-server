-- We deliberately want to ensure the function we add to the rule database
-- has the constraints we need on it when we get it out.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Development.IDE.Graph.Internal.Rules where

import Development.IDE.Graph.Classes
import Control.Exception.Extra
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.Dynamic
import Data.Typeable
import Data.IORef
import qualified Data.HashMap.Strict as Map
import Control.Monad.Trans.Reader
import Development.IDE.Graph.Internal.Types
import Data.Maybe

-- | The type mapping between the @key@ or a rule and the resulting @value@.
--   See 'addBuiltinRule' and 'Development.Shake.Rule.apply'.
type family RuleResult key -- = value

action :: Action a -> Rules ()
action x = do
    ref <- Rules $ asks rulesActions
    liftIO $ modifyIORef' ref (void x:)

addRule
    :: forall key value .
       (RuleResult key ~ value, Typeable key, Hashable key, Eq key, Typeable value)
    => (key -> Maybe BS.ByteString -> RunMode -> Action (RunResult value))
    -> Rules ()
addRule f = do
    ref <- Rules $ asks rulesMap
    liftIO $ modifyIORef' ref $ Map.insert (typeRep (Proxy :: Proxy key)) (toDyn f2)
    where
        f2 :: Key -> Maybe BS.ByteString -> RunMode -> Action (RunResult Value)
        f2 (Key a) b c = do
            v <- f (fromJust $ cast a :: key) b c
            v <- liftIO $ evaluate v
            pure $ (Value . toDyn) <$> v

runRule
    :: TheRules -> Key -> Maybe BS.ByteString -> RunMode -> Action (RunResult Value)
runRule rules key@(Key t) bs mode = case Map.lookup (typeOf t) rules of
    Nothing -> liftIO $ errorIO "Could not find key"
    Just x -> unwrapDynamic x key bs mode

runRules :: Dynamic -> Rules () -> IO (TheRules, [Action ()])
runRules rulesExtra (Rules rules) = do
    rulesActions <- newIORef []
    rulesMap <- newIORef Map.empty
    runReaderT rules SRules{..}
    (,) <$> readIORef rulesMap <*> readIORef rulesActions

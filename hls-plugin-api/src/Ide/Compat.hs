{-# LANGUAGE CPP #-}
module Ide.Compat where

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key        as A
import qualified Data.Aeson.KeyMap     as A
import           Data.Functor.Identity (Identity (..), runIdentity)
#else
import qualified Data.HashMap.Lazy     as Map
#endif
import           Data.Aeson            as A (Value)
import           Data.Text             as T

#if MIN_VERSION_aeson(2,0,0)
adjustJson :: (A.Value -> A.Value) -> A.Key -> A.KeyMap A.Value -> A.KeyMap A.Value
adjustJson f k = runIdentity . A.alterF (Identity . fmap f) k
#else
adjustJson :: (A.Value -> A.Value) -> T.Text -> Map.HashMap T.Text A.Value -> Map.HashMap T.Text A.Value
adjustJson = Map.adjust
#endif

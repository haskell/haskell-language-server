{-# LANGUAGE CPP               #-}
module Ide.Compat where

#if MIN_VERSION_aeson(2,0,0)
import           Data.Aeson.Key             as A (Key)
import qualified Data.Aeson.Key             as A.Key
import qualified Data.Aeson.KeyMap          as Map
#else
import qualified Data.HashMap.Lazy          as Map
#endif
import           Data.Text                  as T

#if MIN_VERSION_aeson(2,0,0)
toJsonKey :: T.Text -> A.Key
toJsonKey = A.Key.fromText
#else
toJsonKey :: T.Text -> T.Text
toJsonKey = id
#endif

#if MIN_VERSION_aeson(2,0,0)
toJsonKey :: T.Text -> A.Key
toJsonKey = A.Key.fromText
#else
toJsonKey :: T.Text -> T.Text
toJsonKey = id
#endif

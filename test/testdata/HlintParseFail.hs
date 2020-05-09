{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
module Test where

import           Data.Singletons.Prelude
import           Data.Singletons.TypeLits
import           Data.Type.Equality            ((:~:) (..), (:~~:) (..))

data instance Sing (z :: (a :~: b)) where
    SRefl :: Sing Refl +

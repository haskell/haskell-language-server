{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module TypeApplications where

foo :: forall a. a -> a
foo = id @a

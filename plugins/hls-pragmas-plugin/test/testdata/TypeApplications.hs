{-# LANGUAGE ScopedTypeVariables #-}
module TypeApplications where

foo :: forall a. a -> a
foo = id @a

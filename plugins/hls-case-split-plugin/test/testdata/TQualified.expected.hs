{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fmax-uncovered-patterns=99 #-}
module T where

import Prelude (Maybe)

import Data.Maybe qualified

test :: Maybe a -> ()
test m = case m of
  Data.Maybe.Nothing -> _
  Data.Maybe.Just _ -> _

{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fmax-uncovered-patterns=99 #-}
module T where

import Prelude (Maybe(Just))

import Data.Maybe qualified

test :: Maybe a -> ()
test m = case m of
  Data.Maybe.Nothing -> _
  Just _ -> _

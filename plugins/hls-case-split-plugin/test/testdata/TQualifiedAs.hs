{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fmax-uncovered-patterns=99 #-}
module T where

import Prelude (Maybe(Just))

import Data.Maybe qualified as M

test :: Maybe a -> ()
test m = case m of

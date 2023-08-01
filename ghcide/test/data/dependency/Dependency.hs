{-# LANGUAGE OverloadedStrings #-}
module Dependency where

import Lucid (Attributes, width_)

width4em :: Attributes
width4em = width_ "4em"

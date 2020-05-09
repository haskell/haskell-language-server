{-# LANGUAGE OverloadedStrings #-}

module Asdf where

import Codec.Compression.GZip

main = return $ compress "hello"
{-# LANGUAGE OverloadedStrings #-}

import Codec.Compression.GZip

main = return $ compress "hello"
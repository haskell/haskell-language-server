{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TCPP where

-- >>> y
-- 11
y :: Integer
y = 11

#define ALL

#ifdef ALL
-- >>> 3+y
-- 14
#else
-- >>> 5+y
#endif

-- >>> 2+y
-- 13

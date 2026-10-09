{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TCPP where

-- >>> y
y :: Integer
y = 11

#define ALL

#ifdef ALL
-- >>> 3+y
#else
-- >>> 5+y
#endif

-- >>> 2+y

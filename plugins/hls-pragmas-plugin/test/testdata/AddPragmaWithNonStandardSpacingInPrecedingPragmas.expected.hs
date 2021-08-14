{-#   OPTIONS_GHC -Wall #-}
{-#       LANGUAGE  OverloadedStrings #-}
{-#      OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# LANGUAGE TupleSections #-}

tupleSection = (1, ) <$> Just 2

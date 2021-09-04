{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
import Data.Monoid

-- some comment
class Semigroup a => SomeData a

instance SomeData All

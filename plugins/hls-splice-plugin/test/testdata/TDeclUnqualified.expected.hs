{-# LANGUAGE TemplateHaskell #-}

module TDeclUnqualified where

import Data.Maybe
import Data.String qualified
import Q

data D = D
instance GHC.Internal.Data.String.IsString D where
  GHC.Internal.Data.String.fromString s
    = GHC.Internal.Data.Maybe.fromMaybe
        D
        (D GHC.Internal.Base.<$
             GHC.Internal.Maybe.Just (GHC.Internal.Data.Ord.Down 1))

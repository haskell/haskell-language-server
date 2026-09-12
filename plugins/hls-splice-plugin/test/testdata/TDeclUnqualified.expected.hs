{-# LANGUAGE TemplateHaskell #-}

module TDeclUnqualified where

import Data.Maybe
import Data.String qualified
import Q

import qualified GHC.Internal.Data.Ord
data D = D
instance Data.String.IsString D where
  fromString s
    = fromMaybe D (D <$ Just (GHC.Internal.Data.Ord.Down 1))

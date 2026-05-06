{-# LANGUAGE TemplateHaskell #-}

module TDeclUnqualified where

import Data.Maybe
import Data.String qualified
import Q

data D = D
instance IsString D where
  fromString s = fromMaybe D (D <$ Just (Down 1))

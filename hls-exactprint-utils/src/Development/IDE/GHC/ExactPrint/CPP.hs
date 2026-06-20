{-# LANGUAGE OverloadedStrings #-}

module Development.IDE.GHC.ExactPrint.CPP
  ( spanHasCpp
  , isCppDirective
  ) where

import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Utf16.Rope.Mixed  (Rope)
import           Development.IDE.Core.Text   (takeLineRange)
import           Language.LSP.Protocol.Types (Position (..), Range (..))

-- | Whether the source over @range@ holds a CPP directive.
spanHasCpp :: Maybe Rope -> Range -> Bool
spanHasCpp Nothing _ = False
spanHasCpp (Just rope) (Range (Position l0 _) (Position l1 _)) =
  any isCppDirective (takeLineRange (fromIntegral l0) (fromIntegral l1) rope)

-- | Whether a line is a CPP directive. In a source compiled with CPP a directive
-- is the only line whose first non-space character is @#@.
isCppDirective :: Text -> Bool
isCppDirective = T.isPrefixOf "#" . T.stripStart

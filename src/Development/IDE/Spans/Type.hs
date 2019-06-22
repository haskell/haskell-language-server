-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- ORIGINALLY COPIED FROM https://github.com/commercialhaskell/intero

-- | Types used separate to GHCi vanilla.

module Development.IDE.Spans.Type(
    SpanInfo(..)
  , SpanSource(..)
  , getNameM
  ) where

import GHC
import Data.Maybe
import OccName


-- | Type of some span of source code. Most of these fields are
-- unboxed but Haddock doesn't show that.
data SpanInfo =
  SpanInfo {spaninfoStartLine :: {-# UNPACK #-} !Int
            -- ^ Start line of the span.
           ,spaninfoStartCol :: {-# UNPACK #-} !Int
            -- ^ Start column of the span.
           ,spaninfoEndLine :: {-# UNPACK #-} !Int
            -- ^ End line of the span (absolute).
           ,spaninfoEndCol :: {-# UNPACK #-} !Int
            -- ^ End column of the span (absolute).
           ,spaninfoType :: !(Maybe Type)
            -- ^ A pretty-printed representation fo the type.
           ,spaninfoSource :: !SpanSource
           -- ^ The actutal 'Name' associated with the span, if
            -- any. This can be useful for accessing a variety of
            -- information about the identifier such as module,
            -- locality, definition location, etc.
           }
instance Show SpanInfo where
  show (SpanInfo sl sc el ec t n) = show [show sl, show sc, show el, show ec, show $ isJust t, show n]

-- we don't always get a name out so sometimes manually annotating source is more appropriate
data SpanSource = Named Name
                | SpanS SrcSpan
                | NoSource
  deriving (Eq)

instance Show SpanSource where
  show = \case
    Named n -> "Named " ++ occNameString (occName n)
    SpanS sp -> "Span " ++ show sp
    NoSource -> "NoSource"

getNameM :: SpanSource -> Maybe Name
getNameM = \case
  Named name -> Just name
  _ -> Nothing

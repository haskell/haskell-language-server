-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings  #-}

-- | Types and functions for working with source code locations.
module Development.IDE.Types.Location
    ( genLocation
    , inRange
    , inRangeClosed
    , isGenLocation
    , Location(..)
    , appendLocation
    , noLocation
    , noRange
    , Position(..)
    , Range(..)
    , appendRange
    ) where

import Control.DeepSeq  (NFData (..))
import Data.Aeson.Types (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Binary (Binary)
import Data.Data
import Data.Text.Prettyprint.Doc.Syntax
import GHC.Generics

------------------------------------------------------------------------------
--- Types
------------------------------------------------------------------------------

-- | Position in a text document expressed as zero-based line and
--   character offset.
data Position = Position
    { positionLine      :: {-# UNPACK #-} !Int
      -- ^ Zero-based line position in the document.
    , positionCharacter :: {-# UNPACK #-} !Int
      -- ^ Zero-based character offset on the line.
    } deriving (Eq, Ord, Read, Show, Generic, Data)

instance NFData Position

instance Pretty Position where
    pretty pos =
        pretty (positionLine pos + 1) <> colon <> pretty (positionCharacter pos + 1)


-- | A range in a text document expressed as inclusive start-position and an
--   exclusive end-position.
data Range = Range
    { rangeStart :: {-# UNPACK #-} !Position
      -- ^ The start position of the range, which is considered to be part of
      -- the range.
    , rangeEnd   :: {-# UNPACK #-} !Position
      -- ^ The end position of the range, which is not considered to be part
      -- of the range.
    } deriving (Eq, Ord, Read, Show, Generic, Data)

instance NFData Range

instance Pretty Range where
    pretty range =
        pretty (rangeStart range) <> "-" <> pretty (rangeEnd range)


-- | Represents a location inside a resource, such as a line inside a text file.
data Location = Location
    { lFilePath :: !FilePath
      -- ^ The uri of the document.
    , lRange    :: !Range
      -- ^ The range within the document.
    } deriving (Eq, Ord, Read, Show, Generic, Data)

instance NFData Location


-- | A dummy location to use when location information is missing.
noLocation :: Location
noLocation = Location
    { lFilePath = "<unknown>"
    , lRange = noRange
    }

-- A dummy range to use when range is unknown
noRange :: Range
noRange =  Range (Position 0 0) (Position 100000 0)


-- | A dummy location to use when location information is not present because
--   the code was generated.
genLocation :: Location
genLocation = Location
    { lFilePath = "<generated>"
    , lRange = Range (Position 0 0) (Position 0 0)
    }


-- | Is a location generated.
isGenLocation :: Location -> Bool
isGenLocation x = lFilePath x == "<generated>"


-- | Check if a position is inside a range.
--   Our definition states that the start of the range is included, but not the end.
inRange :: Position -> Range -> Bool
inRange pos (Range start end) = start <= pos && pos < end


-- | Check if a position is inside a range, including the end.
--   Both start and end of the range are included.
inRangeClosed :: Position -> Range -> Bool
inRangeClosed pos (Range start end) = start <= pos && pos <= end


-- | Produce a new range where the minimum position is the min of both,
--   and the maximum position is the max of both.
appendRange :: Range -> Range -> Range
appendRange r1 r2
 = Range { rangeStart = min (rangeStart r1) (rangeStart r2)
         , rangeEnd   = max (rangeEnd   r1) (rangeEnd   r2) }


-- | Produce a new location where the ranges are the appended and we choose
--   the file path of the second.
appendLocation :: Location -> Location -> Location
appendLocation l1 l2
 = Location { lFilePath = lFilePath l2
            , lRange    = appendRange (lRange l1) (lRange l2) }

instance ToJSON Position
instance FromJSON Position
instance ToJSONKey Position
instance FromJSONKey Position
instance Binary Position

instance ToJSON Range
instance FromJSON Range
instance Binary Range

instance ToJSON Location
instance FromJSON Location
instance Binary Location

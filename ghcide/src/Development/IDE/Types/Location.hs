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
    , noFilePath
    , noRange
    , Position(..)
    , Range(..)
    , appendRange
    ) where

import Language.Haskell.LSP.Types (Location(..), Range(..), Position(..), Uri(..), filePathToUri)

-- | A dummy location to use when location information is missing.
noLocation :: Location
noLocation = Location
    { _uri = filePathToUri noFilePath
    , _range = noRange
    }

noFilePath :: FilePath
noFilePath = "<unknown>"

-- A dummy range to use when range is unknown
noRange :: Range
noRange =  Range (Position 0 0) (Position 100000 0)


-- | A dummy location to use when location information is not present because
--   the code was generated.
genLocation :: Location
genLocation = Location
    { _uri = Uri "<generated>"
    , _range = Range (Position 0 0) (Position 0 0)
    }


-- | Is a location generated.
isGenLocation :: Location -> Bool
isGenLocation x = _uri x == Uri "<generated>"


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
 = Range { _start = min (_start r1) (_start r2)
         , _end   = max (_end   r1) (_end   r2) }


-- | Produce a new location where the ranges are the appended and we choose
--   the file path of the second.
appendLocation :: Location -> Location -> Location
appendLocation l1 l2
 = Location { _uri = _uri l2
            , _range    = appendRange (_range l1) (_range l2) }

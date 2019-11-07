-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0


-- | Types and functions for working with source code locations.
module Development.IDE.Types.Location
    ( Location(..)
    , noFilePath
    , noRange
    , Position(..)
    , showPosition
    , Range(..)
    , Uri(..)
    , NormalizedUri
    , LSP.toNormalizedUri
    , LSP.fromNormalizedUri
    , NormalizedFilePath
    , fromUri
    , toNormalizedFilePath
    , fromNormalizedFilePath
    , filePathToUri
    , filePathToUri'
    , uriToFilePath'
    ) where

import Language.Haskell.LSP.Types (Location(..), Range(..), Position(..))
import Control.DeepSeq
import Data.Binary
import Data.Maybe as Maybe
import Data.Hashable
import Data.String
import System.FilePath
import System.Info.Extra
import qualified Language.Haskell.LSP.Types as LSP
import Language.Haskell.LSP.Types as LSP (
    filePathToUri
  , NormalizedUri(..)
  , Uri(..)
  , toNormalizedUri
  , fromNormalizedUri
  )


-- | Newtype wrapper around FilePath that always has normalized slashes.
newtype NormalizedFilePath = NormalizedFilePath FilePath
    deriving (Eq, Ord, Show, Hashable, NFData, Binary)

instance IsString NormalizedFilePath where
    fromString = toNormalizedFilePath

toNormalizedFilePath :: FilePath -> NormalizedFilePath
toNormalizedFilePath "" = NormalizedFilePath ""
toNormalizedFilePath fp = NormalizedFilePath $ normalise' fp
    where
        -- We do not use System.FilePath’s normalise here since that
        -- also normalises things like the case of the drive letter
        -- which NormalizedUri does not normalise so we get VFS lookup failures.
        normalise' :: FilePath -> FilePath
        normalise' = oneSlash . map (\c -> if isPathSeparator c then pathSeparator else c)

        -- Allow double slashes as the very first element of the path for UNC drives on Windows
        -- otherwise turn adjacent slashes into one. These slashes often arise from dodgy CPP
        oneSlash :: FilePath -> FilePath
        oneSlash (x:xs) | isWindows = x : f xs
        oneSlash xs = f xs

        f (x:y:xs) | isPathSeparator x, isPathSeparator y = f (x:xs)
        f (x:xs) = x : f xs
        f [] = []


fromNormalizedFilePath :: NormalizedFilePath -> FilePath
fromNormalizedFilePath (NormalizedFilePath fp) = fp

-- | We use an empty string as a filepath when we don’t have a file.
-- However, haskell-lsp doesn’t support that in uriToFilePath and given
-- that it is not a valid filepath it does not make sense to upstream a fix.
-- So we have our own wrapper here that supports empty filepaths.
uriToFilePath' :: Uri -> Maybe FilePath
uriToFilePath' uri
    | uri == filePathToUri "" = Just ""
    | otherwise = LSP.uriToFilePath uri

filePathToUri' :: NormalizedFilePath -> NormalizedUri
filePathToUri' = toNormalizedUri . filePathToUri . fromNormalizedFilePath


fromUri :: LSP.NormalizedUri -> NormalizedFilePath
fromUri = toNormalizedFilePath . fromMaybe noFilePath . uriToFilePath' . fromNormalizedUri


noFilePath :: FilePath
noFilePath = "<unknown>"

-- A dummy range to use when range is unknown
noRange :: Range
noRange =  Range (Position 0 0) (Position 100000 0)


showPosition :: Position -> String
showPosition Position{..} = show (_line + 1) ++ ":" ++ show (_character + 1)

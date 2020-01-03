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
import qualified Data.Text as T
import Network.URI
import System.FilePath
import qualified System.FilePath.Posix as FPP
import qualified System.FilePath.Windows as FPW
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
-- We want to keep empty paths instead of normalising them to "."
toNormalizedFilePath "" = NormalizedFilePath ""
toNormalizedFilePath fp = NormalizedFilePath $ normalise fp

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
filePathToUri' (NormalizedFilePath fp) = toNormalizedUri $ Uri $ T.pack $ LSP.fileScheme <> "//" <> platformAdjustToUriPath fp
  where
    -- The definitions below are variants of the corresponding functions in Language.Haskell.LSP.Types.Uri that assume that
    -- the filepath has already been normalised. This is necessary since normalising the filepath has a nontrivial cost.

    toNormalizedUri :: Uri -> NormalizedUri
    toNormalizedUri (Uri t) =
      NormalizedUri $ T.pack $ escapeURIString isUnescapedInURI $ unEscapeString $ T.unpack t

    platformAdjustToUriPath :: FilePath -> String
    platformAdjustToUriPath srcPath
      | isWindows = '/' : escapedPath
      | otherwise = escapedPath
      where
        (splitDirectories, splitDrive)
          | isWindows =
              (FPW.splitDirectories, FPW.splitDrive)
          | otherwise =
              (FPP.splitDirectories, FPP.splitDrive)
        escapedPath =
            case splitDrive srcPath of
                (drv, rest) ->
                    convertDrive drv `FPP.joinDrive`
                    FPP.joinPath (map (escapeURIString unescaped) $ splitDirectories rest)
        -- splitDirectories does not remove the path separator after the drive so
        -- we do a final replacement of \ to /
        convertDrive drv
          | isWindows && FPW.hasTrailingPathSeparator drv =
              FPP.addTrailingPathSeparator (init drv)
          | otherwise = drv
        unescaped c
          | isWindows = isUnreserved c || c `elem` [':', '\\', '/']
          | otherwise = isUnreserved c || c == '/'



fromUri :: LSP.NormalizedUri -> NormalizedFilePath
fromUri = toNormalizedFilePath . fromMaybe noFilePath . uriToFilePath' . fromNormalizedUri


noFilePath :: FilePath
noFilePath = "<unknown>"

-- A dummy range to use when range is unknown
noRange :: Range
noRange =  Range (Position 0 0) (Position 100000 0)


showPosition :: Position -> String
showPosition Position{..} = show (_line + 1) ++ ":" ++ show (_character + 1)

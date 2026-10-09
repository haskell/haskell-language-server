-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | The OsPath-backed path type shared by HLS and its plugins.
--
-- This lives in hls-plugin-api (not ghcide) because ghcide depends on
-- hls-plugin-api; plugins receive 'NormalizedOsPath' through both.
module Ide.Types.Location
    ( NormalizedOsPath(..)
    , systemFsEncoding
    , encodeOsPath
    , decodeOsPath
    , toNormalizedFilePath'
    , fromNormalizedFilePath
    , uriToFilePath'
    , uriToNormalizedOsPath
    , filePathToUri'
    , fromUri
    , emptyFilePath
    , emptyPathUri
    , noFilePath
    ) where

import           Control.DeepSeq             (NFData (..))
import           Data.Binary                 (Binary)
import qualified Data.Binary                 as Bin (get, put)
import           Data.Functor                ((<&>))
import           Data.Hashable               (Hashable (..))
import           Data.Maybe                  (fromMaybe)
import           Data.String
import qualified Language.LSP.Protocol.Types as LSP
import           System.FilePath             (normalise)
import           System.IO.Unsafe            (unsafePerformIO)
import qualified System.OsPath               as OsPath
import           System.OsPath               (OsPath)
import           System.OsPath.Encoding      (EncodingException)
#if !MIN_VERSION_filepath(1,5,0)
import           System.OsPath.Encoding      (utf16le_b)
#endif

import           GHC.IO.Encoding             (TextEncoding,
                                              getFileSystemEncoding)

-- | A file path in the platform's native representation (ShortByteString),
-- paired with its cached 'LSP.NormalizedUri'. Performance-critical: hashed via
-- the cached Uri; do not modify without profiling.
data NormalizedOsPath = NormalizedOsPath !LSP.NormalizedUri {-# UNPACK #-} !OsPath
  deriving stock (Eq, Ord)

-- | The filesystem encoding (PEP-383 surrogates on POSIX, UTF-16 on Windows).
systemFsEncoding :: TextEncoding
systemFsEncoding = unsafePerformIO getFileSystemEncoding
{-# NOINLINE systemFsEncoding #-}

encodeOsPath :: FilePath -> Either EncodingException OsPath
#if MIN_VERSION_filepath(1,5,0)
-- filepath >= 1.5's encodeWith applies its encodings unconditionally (even on
-- POSIX), so use the filesystem-encoding aware encodeFS instead. It is total:
-- the PEP-383 surrogate roundtrip cannot fail on POSIX, and UTF-16 encoding
-- cannot fail on Windows.
encodeOsPath = Right . unsafePerformIO . OsPath.encodeFS
#else
-- filepath 1.4.3xx (GHC <= 9.8) takes the Windows UTF-16 encoding as a second
-- argument; it is only used on Windows and must roundtrip through decodeWith.
encodeOsPath = OsPath.encodeWith systemFsEncoding windowsEnc
#endif

decodeOsPath :: OsPath -> Either EncodingException FilePath
#if MIN_VERSION_filepath(1,5,0)
-- Mirrors encodeOsPath: decodeFS is total for PEP-383/UTF-16 encoded paths.
decodeOsPath osp = Right (unsafePerformIO (OsPath.decodeFS osp))
{-# NOINLINE decodeOsPath #-}
#else
decodeOsPath = OsPath.decodeWith systemFsEncoding windowsEnc
#endif

#if !MIN_VERSION_filepath(1,5,0)
windowsEnc :: TextEncoding
windowsEnc = utf16le_b
{-# NOINLINE windowsEnc #-}
#endif

encodingError :: String -> EncodingException -> a
encodingError what e = error (what ++ ": " ++ show e)
{-# NOINLINE encodingError #-}

toNormalizedFilePath' :: FilePath -> NormalizedOsPath
-- We want to keep empty paths instead of normalising them to "."
toNormalizedFilePath' "" = emptyFilePath
toNormalizedFilePath' fp =
  let s = normalise fp
      nuri = LSP.toNormalizedUri (LSP.filePathToUri s)
      osp = either (encodingError "toNormalizedFilePath'") id (encodeOsPath s)
  in NormalizedOsPath nuri osp

fromNormalizedFilePath :: NormalizedOsPath -> FilePath
-- Invariant: every 'NormalizedOsPath' roundtrips through the filesystem
-- encoding (POSIX PEP-383 is byte-exact), so the error is unreachable.
fromNormalizedFilePath (NormalizedOsPath _ osp) =
  either (encodingError "fromNormalizedFilePath") id (decodeOsPath osp)

-- | We use an empty string as a filepath when we don’t have a file.
-- However, haskell-lsp doesn’t support that in uriToFilePath and given
-- that it is not a valid filepath it does not make sense to upstream a fix.
-- So we have our own wrapper here that supports empty filepaths.
uriToFilePath' :: LSP.Uri -> Maybe OsPath
uriToFilePath' uri
    | uri == LSP.fromNormalizedUri emptyPathUri = Just mempty
    | otherwise = LSP.uriToFilePath uri >>= either (const Nothing) Just . encodeOsPath

-- | 'fromUri' but 'Nothing' when the Uri does not denote a file path or the
-- decoded path cannot be represented in the filesystem encoding (e.g. a
-- non-ASCII path under a LANG=C locale). Total on client-supplied input.
uriToNormalizedOsPath :: LSP.Uri -> Maybe NormalizedOsPath
uriToNormalizedOsPath uri = do
  fp <- LSP.uriToFilePath uri
  let s = normalise fp
  osp <- either (const Nothing) Just (encodeOsPath s)
  let nuri = LSP.toNormalizedUri (LSP.filePathToUri s)
  pure (NormalizedOsPath nuri osp)

-- | O(1): the Uri is cached in the path.
filePathToUri' :: NormalizedOsPath -> LSP.NormalizedUri
filePathToUri' (NormalizedOsPath uri _) = uri

fromUri :: LSP.NormalizedUri -> NormalizedOsPath
fromUri nuri = fromMaybe (toNormalizedFilePath' noFilePath)
             $ LSP.uriToNormalizedFilePath nuri <&> toNormalizedFilePath' . LSP.fromNormalizedFilePath

emptyPathUri :: LSP.NormalizedUri
emptyPathUri =
    let s = "file://"
    in LSP.NormalizedUri (hash s) s

emptyFilePath :: NormalizedOsPath
emptyFilePath = NormalizedOsPath emptyPathUri mempty

noFilePath :: FilePath
noFilePath = "<unknown>"

-- Hashing uses the cached Uri: identical behaviour to lsp's Text-backed type.
instance Hashable NormalizedOsPath where
  hash (NormalizedOsPath uri _) = hash uri
  hashWithSalt s (NormalizedOsPath uri _) = hashWithSalt s uri

instance NFData NormalizedOsPath where
  rnf (NormalizedOsPath uri fp) = rnf uri `seq` fp `seq` ()

instance Show NormalizedOsPath where
  show p = "NormalizedFilePath " ++ show (fromNormalizedFilePath p)

instance IsString NormalizedOsPath where
  fromString = toNormalizedFilePath'

instance Binary NormalizedOsPath where
  put = Bin.put . fromNormalizedFilePath
  get = toNormalizedFilePath' <$> Bin.get

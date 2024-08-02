module Development.IDE.Types.Path
  (Abs, Rel, normalizeAbs, mkAbsPath, Path)
where

import           Language.LSP.Protocol.Types


data Abs
data Rel

newtype Path a b = MkPath { getRawPath :: b } deriving (Eq, Show)

normalizeAbs :: Path Abs NormalizedFilePath -> NormalizedFilePath
normalizeAbs = getRawPath

-- | TODO: guarantee that path is absolute
mkAbsPath :: NormalizedFilePath -> Path Abs NormalizedFilePath
mkAbsPath path = MkPath path

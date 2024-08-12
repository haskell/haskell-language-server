module Development.IDE.Types.Path
  (Abs, Rel, mkAbsPath, Path, fromAbsPath, mkAbsFromFp, prettyAbsPath, absToUri, emptyAbsPath, removeSuffix, addSuffix)
where

import           Control.DeepSeq
import           Data.Hashable
import           Data.List.Extra                as L
import           Development.IDE.Types.Location
import qualified Language.LSP.Protocol.Types    as LSP

import           Prettyprinter.Internal
import           Prettyprinter.Render.Terminal  as Terminal

data Abs
data Rel

newtype Path a b = Path { getRawPath :: b } deriving (Eq, Show, Ord, Hashable, NFData)

prettyAbsPath :: Path Abs NormalizedFilePath -> Doc Terminal.AnsiStyle
prettyAbsPath (Path x) = pretty (show x)

-- | TODO: guarantee that path is absolute
mkAbsPath :: NormalizedFilePath -> Path Abs NormalizedFilePath
mkAbsPath path = Path path

mkAbsFromFp :: FilePath -> Path Abs NormalizedFilePath
mkAbsFromFp = mkAbsPath . toNormalizedFilePath'

fromAbsPath :: Path Abs NormalizedFilePath -> FilePath
fromAbsPath = fromNormalizedFilePath . getRawPath

absToUri :: Path Abs NormalizedFilePath -> LSP.NormalizedUri
absToUri = LSP.normalizedFilePathToUri . getRawPath

emptyAbsPath :: Path Abs NormalizedFilePath
emptyAbsPath = mkAbsPath LSP.emptyNormalizedFilePath

-- | remove last suffix of length s from supplied path
removeSuffix :: Path Abs NormalizedFilePath -> Int -> Path Abs NormalizedFilePath
removeSuffix (Path f) s = mkAbsFromFp $ L.dropEnd s $ fromNormalizedFilePath f

addSuffix :: Path Abs NormalizedFilePath -> String -> Path Abs NormalizedFilePath
addSuffix (Path f) s = mkAbsFromFp $ fromNormalizedFilePath f ++ s

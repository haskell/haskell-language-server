module Development.IDE.Core.AbstractPath where

import           System.FilePath

data AbstractPath = RelativePath FilePath
                    | AbsolutePath FilePath
                    deriving (Show)

mkAbstract :: FilePath -> AbstractPath
mkAbstract x | isRelative x = RelativePath x
             | otherwise = AbsolutePath x

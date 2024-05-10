-- | Fake cabal module for local building

module PathsHlsGraph(getDataDir, version) where

import           Data.Version.Extra

-- If hls_graph can't find files in the data directory it tries relative to the executable
getDataDir :: IO FilePath
getDataDir = pure "random_path_that_cannot_possibly_exist"

version :: Version
version = makeVersion [0,0]

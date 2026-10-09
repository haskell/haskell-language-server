module SessionCacheDirTests (tests) where

import           Control.Monad
import           Development.IDE.Session.Ghc
import           GHC.ResponseFile
import           System.Directory
import           System.FilePath
import           System.IO.Extra
import           Test.Tasty
import           Test.Tasty.HUnit

type Unit = (String, [String])

unitA, unitB :: Unit
unitA = ("unit-a", ["-this-unit-id", "unit-a"])
unitB = ("unit-b", ["-this-unit-id", "unit-b"])

withOpt :: String -> Unit -> Unit
withOpt opt (name, opts) = (name, opts ++ [opt])

-- | Write example response files, mocking cabal multi-repl's command line.
multiReplOptions :: FilePath -> [Unit] -> IO [String]
multiReplOptions dir units = do
  createDirectoryIfMissing True dir
  fmap concat . forM units $ \(name, opts) -> do
    writeFile (dir </> name) (escapeArgs opts)
    pure ["-unit", "@" ++ (dir </> name)]

-- | The interface cache folder a session with these options would land in.
cacheKey :: [String] -> IO (Maybe FilePath)
cacheKey opts =
  hiCacheDir . getCacheDirsIn "/cache" "unit-a" Nothing
    <$> cacheDirOptions mempty "/" opts

-- | Compare the keys of two potential sessions containing differing units.
twoSessions :: TestName -> (Maybe FilePath -> Maybe FilePath -> Assertion) -> [Unit] -> [Unit] -> TestTree
twoSessions name compare' first second = testCase name $ withTempDir $ \tmp -> do
  a <- cacheKey =<< multiReplOptions (tmp </> "out-1") first
  b <- cacheKey =<< multiReplOptions (tmp </> "out-2") second
  compare' a b

keepsKey, movesKey :: Maybe FilePath -> Maybe FilePath -> Assertion
keepsKey = assertEqual "cache key moved"
movesKey a b = assertBool "cache key kept" (a /= b)

tests :: TestTree
tests =
  testGroup
    "SessionCacheDir"
    [ twoSessions "a new response file directory does not invalidate" keepsKey
        [unitA, unitB]
        [unitA, unitB],
      twoSessions "reordering the units does not invalidate" keepsKey
        [unitA, unitB]
        [unitB, unitA],
      twoSessions "changed options inside a response file invalidates" movesKey
        [unitA, unitB]
        [withOpt "-O2" unitA, unitB],
      twoSessions "swapping options between units invalidates" movesKey
        [withOpt "-O0" unitA, withOpt "-O2" unitB]
        [withOpt "-O2" unitA, withOpt "-O0" unitB],
      twoSessions "reordering options inside a unit invalidates" movesKey
        [withOpt "-XNoImplicitPrelude" (withOpt "-XImplicitPrelude" unitA), unitB]
        [withOpt "-XImplicitPrelude" (withOpt "-XNoImplicitPrelude" unitA), unitB],
      testCase "changed options outside a response file invalidates" $
        withTempDir $ \tmp -> do
          opts <- multiReplOptions (tmp </> "out-1") [unitA, unitB]
          a <- cacheKey opts
          b <- cacheKey (opts ++ ["-O2"])
          movesKey a b
    ]

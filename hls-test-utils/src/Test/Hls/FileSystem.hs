{-# LANGUAGE OverloadedStrings #-}
module Test.Hls.FileSystem
  ( FileSystem(..)
  , VirtualFileTree(..)
  , FileTree
  , Content
  -- * init
  , materialise
  , materialiseVFT
  -- * Interaction
  , readFileFS
  , writeFileFS
  -- * Test helpers
  , mkVirtualFileTree
  , toNfp
  , toAbsFp
  -- * Builders
  , file
  , copy
  , directory
  , text
  , ref
  -- * Cradle helpers
  , directCradle
  , simpleCabalCradle
  -- * Full project setups
  , directProject
  , directProjectMulti
  , simpleCabalProject
  , simpleCabalProject'
  ) where

import           Data.Foldable               (traverse_)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           Development.IDE             (NormalizedFilePath)
import           Language.LSP.Protocol.Types (toNormalizedFilePath)
import           System.Directory
import           System.FilePath             as FP

-- ----------------------------------------------------------------------------
-- Top Level definitions
-- ----------------------------------------------------------------------------

-- | Representation of a 'VirtualFileTree' that has been 'materialise'd to disk.
--
data FileSystem =
  FileSystem
    { fsRoot         :: FilePath
    , fsTree         :: [FileTree]
    , fsOriginalRoot :: FilePath
    } deriving (Eq, Ord, Show)

-- | Virtual representation of a filesystem tree.
--
-- Operations of 'vftTree' are relative to 'vftOriginalRoot'.
-- In particular, any 'copy' etc. operation looks for the sources in 'vftOriginalRoot'.
--
-- To persist a 'VirtualFileTree', look at 'materialise' and 'materialiseVFT'.
data VirtualFileTree =
  VirtualFileTree
    { vftTree         :: [FileTree]
    , vftOriginalRoot :: FilePath
    } deriving (Eq, Ord, Show)

data FileTree
  = File FilePath Content
  | Directory FilePath [FileTree]
  deriving (Show, Eq, Ord)

data Content
  = Inline T.Text
  | Ref FilePath
  deriving (Show, Eq, Ord)

-- ----------------------------------------------------------------------------
-- API with side effects
-- ----------------------------------------------------------------------------

readFileFS :: FileSystem -> FilePath -> IO T.Text
readFileFS fs fp = do
  T.readFile (fsRoot fs </> FP.normalise fp)

writeFileFS :: FileSystem -> FilePath -> Content -> IO ()
writeFileFS fs fp content = do
  contents <- case content of
    Inline txt -> pure txt
    Ref path   -> T.readFile (fsOriginalRoot fs </> FP.normalise path)
  T.writeFile (fsRoot fs </> FP.normalise fp) contents

-- | Materialise a virtual file tree in the 'rootDir' directory.
--
-- Synopsis: @'materialise' rootDir fileTree testDataDir@
--
-- File references in '[FileTree]' are resolved relative to the @testDataDir@.
materialise :: FilePath -> [FileTree] -> FilePath -> IO FileSystem
materialise rootDir' fileTree testDataDir' = do
  let testDataDir = FP.normalise testDataDir'
      rootDir = FP.normalise rootDir'

      persist :: FilePath -> FileTree -> IO ()
      persist fp (File name cts) = case cts of
        Inline txt -> T.writeFile (fp </> name) txt
        Ref path -> copyFile (testDataDir </> FP.normalise path) (fp </> takeFileName name)
      persist fp (Directory name nodes) = do
        createDirectory (fp </> name)
        mapM_ (persist (fp </> name)) nodes

  traverse_ (persist rootDir) fileTree
  pure $ FileSystem rootDir fileTree testDataDir

-- | Materialise a virtual file tree in the 'rootDir' directory.
--
-- Synopsis: @'materialiseVFT' rootDir virtualFileTree@
--
-- File references in 'virtualFileTree' are resolved relative to the @vftOriginalRoot@.
materialiseVFT :: FilePath -> VirtualFileTree -> IO FileSystem
materialiseVFT root fs =
  materialise root (vftTree fs) (vftOriginalRoot fs)

-- ----------------------------------------------------------------------------
-- Test definition helpers
-- ----------------------------------------------------------------------------

mkVirtualFileTree :: FilePath -> [FileTree] -> VirtualFileTree
mkVirtualFileTree testDataDir tree =
  VirtualFileTree
    { vftTree = tree
    , vftOriginalRoot = testDataDir
    }

toAbsFp :: FileSystem -> FilePath -> FilePath
toAbsFp fs fp = fsRoot fs </> FP.normalise fp

toNfp :: FileSystem -> FilePath -> NormalizedFilePath
toNfp fs fp =
  toNormalizedFilePath $ toAbsFp fs fp

-- ----------------------------------------------------------------------------
-- Builders
-- ----------------------------------------------------------------------------

-- | Create a file in the test project with some content.
--
-- Only the filename will be used, and any directory components are *not*
-- reflected in the test project.
file :: FilePath -> Content -> FileTree
file fp cts = File fp cts

-- | Copy a filepath into a test project. The name of the file is also used
-- in the test project.
--
-- The filepath is always resolved to the root of the test data dir.
copy :: FilePath -> FileTree
copy fp = File fp (Ref fp)

directory :: FilePath -> [FileTree] -> FileTree
directory name nodes = Directory name nodes

-- | Write the given test directly into a file.
text :: T.Text -> Content
text = Inline

-- | Read the contents of the given file
-- The filepath is always resolved to the root of the test data dir.
ref :: FilePath -> Content
ref = Ref

-- ----------------------------------------------------------------------------
-- Cradle Helpers
-- ----------------------------------------------------------------------------

-- | Set up a simple direct cradle.
--
-- All arguments are added to the direct cradle file.
-- Arguments will not be escaped.
directCradle :: [T.Text] -> FileTree
directCradle args =
  file "hie.yaml"
    ( Inline $ T.unlines $
      [ "cradle:"
      , "  direct:"
      , "    arguments:"
      ] <>
      [ "    - " <> arg | arg <- args]
    )

-- | Set up a simple cabal cradle.
--
-- Prefer simple cabal cradle, over custom multi cabal cradles if possible.
simpleCabalCradle :: FileTree
simpleCabalCradle =
  file "hie.yaml"
    (Inline $ T.unlines
      [ "cradle:"
      , "  cabal:"
      ]
    )


-- ----------------------------------------------------------------------------
-- Project setup builders
-- ----------------------------------------------------------------------------

-- | Set up a test project with a single haskell file.
directProject :: FilePath -> [FileTree]
directProject fp =
  [ directCradle [T.pack fp]
  , file fp (Ref fp)
  ]

-- | Set up a test project with multiple haskell files.
--
directProjectMulti :: [FilePath] -> [FileTree]
directProjectMulti fps =
  [ directCradle $ fmap T.pack fps
  ] <> fmap copy fps

-- | Set up a simple cabal cradle  project and copy all the given filepaths
-- into the test directory.
simpleCabalProject :: [FilePath] -> [FileTree]
simpleCabalProject fps =
  [ simpleCabalCradle
  ] <> fmap copy fps

-- | Set up a simple cabal cradle project.
simpleCabalProject' :: [FileTree] -> [FileTree]
simpleCabalProject' fps =
  [ simpleCabalCradle
  ] <> fps

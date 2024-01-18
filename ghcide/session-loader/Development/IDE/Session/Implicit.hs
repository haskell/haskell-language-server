module Development.IDE.Session.Implicit
  ( loadImplicitCradle
  ) where


import           Control.Applicative       ((<|>))
import           Control.Exception         (handleJust)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Bifunctor
import           Data.Functor              ((<&>))
import           Data.Maybe
import           Data.Void
import           System.Directory          hiding (findFile)
import           System.FilePath
import           System.IO.Error

import           Colog.Core                (LogAction (..), WithSeverity (..))
import           HIE.Bios.Config
import           HIE.Bios.Cradle           (defaultCradle, getCradle)
import           HIE.Bios.Types            hiding (ActionName (..))

import           Hie.Cabal.Parser
import           Hie.Locate
import qualified Hie.Yaml                  as Implicit

loadImplicitCradle :: Show a => LogAction IO (WithSeverity Log) -> FilePath -> IO (Cradle a)
loadImplicitCradle l wfile = do
  is_dir <- doesDirectoryExist wfile
  let wdir | is_dir = wfile
           | otherwise = takeDirectory wfile
  cfg <- runMaybeT (implicitConfig wdir)
  case cfg of
    Just bc -> getCradle l absurd bc
    Nothing -> return $ defaultCradle l wdir

-- | Wraps up the cradle inferred by @inferCradleTree@ as a @CradleConfig@ with no dependencies
implicitConfig :: FilePath -> MaybeT IO (CradleConfig a, FilePath)
implicitConfig = (fmap . first) (CradleConfig noDeps) . inferCradleTree
  where
  noDeps :: [FilePath]
  noDeps = []


inferCradleTree :: FilePath -> MaybeT IO (CradleTree a, FilePath)
inferCradleTree start_dir =
       maybeItsBios
   -- If we have both a config file (cabal.project/stack.yaml) and a work dir
   -- (dist-newstyle/.stack-work), prefer that
   <|> (cabalExecutable >> cabalConfigDir start_dir >>= \dir -> cabalWorkDir dir >> pure (simpleCabalCradle dir))
   <|> (stackExecutable >> stackConfigDir start_dir >>= \dir -> stackWorkDir dir >> stackCradle dir)
   -- If we have a cabal.project OR we have a .cabal and dist-newstyle, prefer cabal
   <|> (cabalExecutable >> (cabalConfigDir start_dir <|> cabalFileAndWorkDir) <&> simpleCabalCradle)
   -- If we have a stack.yaml, use stack
   <|> (stackExecutable >> stackConfigDir start_dir >>= stackCradle)
   -- If we have a cabal file, use cabal
   <|> (cabalExecutable >> cabalFileDir start_dir <&> simpleCabalCradle)

  where
  maybeItsBios = (\wdir -> (Bios (Program $ wdir </> ".hie-bios") Nothing Nothing, wdir)) <$> biosWorkDir start_dir

  cabalFileAndWorkDir = cabalFileDir start_dir >>= (\dir -> cabalWorkDir dir >> pure dir)

-- | Generate a stack cradle given a filepath.
--
-- Since we assume there was proof that this file belongs to a stack cradle
-- we look immediately for the relevant @*.cabal@ and @stack.yaml@ files.
-- We do not look for package.yaml, as we assume the corresponding .cabal has
-- been generated already.
--
-- We parse the @stack.yaml@ to find relevant @*.cabal@ file locations, then
-- we parse the @*.cabal@ files to generate a mapping from @hs-source-dirs@ to
-- component names.
stackCradle :: FilePath -> MaybeT IO (CradleTree a, FilePath)
stackCradle fp = do
  pkgs <- stackYamlPkgs fp
  pkgsWithComps <- liftIO $ catMaybes <$> mapM (nestedPkg fp) pkgs
  let yaml = fp </> "stack.yaml"
  pure $ (,fp) $ case pkgsWithComps of
    [] -> Stack (StackType Nothing (Just yaml))
    ps -> StackMulti mempty $ do
      Package n cs <- ps
      c <- cs
      let (prefix, comp) = Implicit.stackComponent n c
      pure (prefix, StackType (Just comp) (Just yaml))

-- | By default, we generate a simple cabal cradle which is equivalent to the
-- following hie.yaml:
--
-- @
--   cradle:
--     cabal:
-- @
--
-- Note, this only works reliable for reasonably modern cabal versions >= 3.2.
simpleCabalCradle :: FilePath -> (CradleTree a, FilePath)
simpleCabalCradle fp = (Cabal $ CabalType Nothing Nothing, fp)

cabalExecutable :: MaybeT IO FilePath
cabalExecutable = MaybeT $ findExecutable "cabal"

stackExecutable :: MaybeT IO FilePath
stackExecutable = MaybeT $ findExecutable "stack"

biosWorkDir :: FilePath -> MaybeT IO FilePath
biosWorkDir = findFileUpwards (".hie-bios" ==)

cabalWorkDir :: FilePath -> MaybeT IO ()
cabalWorkDir wdir = do
  check <- liftIO $ doesDirectoryExist (wdir </> "dist-newstyle")
  unless check $ fail "No dist-newstyle"

stackWorkDir :: FilePath -> MaybeT IO ()
stackWorkDir wdir = do
  check <- liftIO $ doesDirectoryExist (wdir </> ".stack-work")
  unless check $ fail "No .stack-work"

cabalConfigDir :: FilePath -> MaybeT IO FilePath
cabalConfigDir = findFileUpwards (\fp -> fp == "cabal.project" || fp == "cabal.project.local")

cabalFileDir :: FilePath -> MaybeT IO FilePath
cabalFileDir = findFileUpwards (\fp -> takeExtension fp == ".cabal")

stackConfigDir :: FilePath -> MaybeT IO FilePath
stackConfigDir = findFileUpwards isStack
  where
    isStack name = name == "stack.yaml"

-- | Searches upwards for the first directory containing a file to match
-- the predicate.
findFileUpwards :: (FilePath -> Bool) -> FilePath -> MaybeT IO FilePath
findFileUpwards p dir = do
  cnts <-
    liftIO
    $ handleJust
        -- Catch permission errors
        (\(e :: IOError) -> if isPermissionError e then Just [] else Nothing)
        pure
        (findFile p dir)

  case cnts of
    [] | dir' == dir -> fail "No cabal files"
            | otherwise   -> findFileUpwards p dir'
    _ : _ -> return dir
  where dir' = takeDirectory dir

-- | Sees if any file in the directory matches the predicate
findFile :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findFile p dir = do
  b <- doesDirectoryExist dir
  if b then getFiles >>= filterM doesPredFileExist else return []
  where
    getFiles = filter p <$> getDirectoryContents dir
    doesPredFileExist file = doesFileExist $ dir </> file

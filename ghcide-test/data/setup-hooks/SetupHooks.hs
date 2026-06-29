{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StaticPointers     #-}

module SetupHooks where

-- Cabal
import Distribution.Compat.Binary
import Distribution.Simple.LocalBuildInfo  (interpretSymbolicPathLBI)
import Distribution.Simple.SetupHooks
import Distribution.Simple.Utils           (rewriteFileEx)
import Distribution.Utils.Path
import Distribution.Verbosity              (normal, mkVerbosity, defaultVerbosityHandles)

-- base
import Control.Monad.IO.Class              (liftIO)
import Data.List                           (isSuffixOf)
import qualified Data.List.NonEmpty as NE  (NonEmpty (..))
import Data.String                         (fromString)
import GHC.Generics

-- directory
import System.Directory                    (listDirectory)

-- filepath
import System.FilePath                     (dropExtension)

-- This import is unnecessary, but it's kept around so that this file would
-- fail to compile were we to use a version of Cabal that doesn't write out
-- a pre-build rule manifest file.
import Distribution.Simple.BuildPaths (preBuildMonitorManifestFile)

--------------------------------------------------------------------------------

setupHooks :: SetupHooks
setupHooks = noSetupHooks
  { buildHooks = noBuildHooks
    { preBuildComponentRules = Just $ rules (static ()) preBuildRules } }

preBuildRules :: PreBuildComponentInputs -> RulesM ()
preBuildRules (PreBuildComponentInputs { localBuildInfo = lbi, targetInfo = tgt }) = do
  let clbi       = targetCLBI tgt
      autogenDir = autogenComponentModulesDir lbi clbi
      srcDir     = sameDirectory
  allFiles <- liftIO $ listDirectory (interpretSymbolicPathLBI lbi srcDir)
  mapM_ (registerMyPP srcDir autogenDir) (filter (".myPP" `isSuffixOf`) allFiles)

registerMyPP
  :: SymbolicPath Pkg (Dir Source)
  -> SymbolicPath Pkg (Dir Source)
  -> FilePath
  -> RulesM ()
registerMyPP srcDir autogenDir fileName =
  let baseName = dropExtension fileName
  in registerRule_ (fromString $ "myPP " ++ baseName) $
       staticRule
         (mkCommand (static Dict) (static runMyPP) $
           MyPPInput { ppSrcDir = srcDir, ppAutogenDir = autogenDir, ppBaseName = baseName })
         [ FileDependency $ Location srcDir (makeRelativePathEx fileName) ]
         ( Location autogenDir (makeRelativePathEx baseName <.> "hs") NE.:| [] )

runMyPP :: MyPPInput -> IO ()
runMyPP MyPPInput{..} = do
  content <- readFile (getSymbolicPath ppSrcDir </> ppBaseName <.> "myPP")
  rewriteFileEx (mkVerbosity defaultVerbosityHandles normal)
    (getSymbolicPath ppAutogenDir </> ppBaseName <.> "hs") $
    "module " ++ ppBaseName ++ " where\n" ++ content

data MyPPInput = MyPPInput
  { ppSrcDir     :: SymbolicPath Pkg (Dir Source)
  , ppAutogenDir :: SymbolicPath Pkg (Dir Source)
  , ppBaseName   :: String
  } deriving stock    (Show, Generic)
    deriving anyclass Binary

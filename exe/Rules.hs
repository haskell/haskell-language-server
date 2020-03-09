module Rules
  ( loadGhcSession
  , cradleToSession
  , cradleLoadedMethod
  , createSession
  , getComponentOptions
  )
where

import           Control.Exception
import           Control.Monad                  (filterM, when)
import qualified Crypto.Hash.SHA1               as H
import           Data.ByteString.Base16         (encode)
import qualified Data.ByteString.Char8          as B
import           Data.Functor                   ((<&>))
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text)
import           Development.IDE.Core.Rules     (defineNoFile)
import           Development.IDE.Core.Service   (getIdeOptions)
import           Development.IDE.Core.Shake     (sendEvent, define, useNoFile_)
import           Development.IDE.GHC.Util
import           Development.IDE.Types.Location (fromNormalizedFilePath)
import           Development.IDE.Types.Options  (IdeOptions(IdeOptions, optTesting))
import           Development.Shake
import           DynFlags                       (gopt_set, gopt_unset,
                                                 updOptLevel)
import           GHC
import qualified GHC.Paths
import           HIE.Bios
import           HIE.Bios.Cradle
import           HIE.Bios.Environment           (addCmdOpts)
import           HIE.Bios.Types
import           Linker                         (initDynLinker)
import           RuleTypes
import qualified System.Directory.Extra         as IO
import           System.Environment             (lookupEnv)
import           System.FilePath.Posix          (addTrailingPathSeparator,
                                                 (</>))
import Language.Haskell.LSP.Messages as LSP
import Language.Haskell.LSP.Types as LSP
import Data.Aeson (ToJSON(toJSON))

-- Prefix for the cache path
cacheDir :: String
cacheDir = "ghcide"

notifyCradleLoaded :: FilePath -> LSP.FromServerMessage
notifyCradleLoaded fp =
    LSP.NotCustomServer $
    LSP.NotificationMessage "2.0" (LSP.CustomServerMethod cradleLoadedMethod) $
    toJSON fp

loadGhcSession :: Rules ()
loadGhcSession =
    -- This rule is for caching the GHC session. E.g., even when the cabal file
    -- changed, if the resulting flags did not change, we would continue to use
    -- the existing session.
    defineNoFile $ \(GetHscEnv opts deps) ->
        liftIO $ createSession $ ComponentOptions opts deps

cradleToSession :: Rules ()
cradleToSession = define $ \LoadCradle nfp -> do
    let f = fromNormalizedFilePath nfp

    IdeOptions{optTesting} <- getIdeOptions

    -- If the path points to a directory, load the implicit cradle
    mbYaml <- doesDirectoryExist f <&> \isDir -> if isDir then Nothing else Just f
    cradle <- liftIO $  maybe (loadImplicitCradle $ addTrailingPathSeparator f) loadCradle mbYaml

    when optTesting $
        sendEvent $ notifyCradleLoaded f

    cmpOpts <- liftIO $ getComponentOptions cradle
    let opts = componentOptions cmpOpts
        deps = componentDependencies cmpOpts
        deps' = case mbYaml of
                  -- For direct cradles, the hie.yaml file itself must be watched.
                  Just yaml | isDirectCradle cradle -> yaml : deps
                  _                                 -> deps
    existingDeps <- filterM doesFileExist deps'
    need existingDeps
    ([],) . pure <$> useNoFile_ (GetHscEnv opts deps)

cradleLoadedMethod :: Text
cradleLoadedMethod = "ghcide/cradle/loaded"

getComponentOptions :: Cradle a -> IO ComponentOptions
getComponentOptions cradle = do
    let showLine s = putStrLn ("> " ++ s)
    -- WARNING 'runCradle is very expensive and must be called as few times as possible
    cradleRes <- runCradle (cradleOptsProg cradle) showLine ""
    case cradleRes of
        CradleSuccess r -> pure r
        CradleFail err  -> throwIO err
        -- TODO Rather than failing here, we should ignore any files that use this cradle.
        -- That will require some more changes.
        CradleNone      -> fail "'none' cradle is not yet supported"

createSession :: ComponentOptions -> IO HscEnvEq
createSession (ComponentOptions theOpts _) = do
    libdir <- getLibdir

    cacheDir <- getCacheDir theOpts

    env <- runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        (dflags', _targets) <- addCmdOpts theOpts dflags
        _ <- setSessionDynFlags $
             -- disabled, generated directly by ghcide instead
             flip gopt_unset Opt_WriteInterface $
             -- disabled, generated directly by ghcide instead
             -- also, it can confuse the interface stale check
             dontWriteHieFiles $
             setHiDir cacheDir $
             setDefaultHieDir cacheDir $
             setIgnoreInterfacePragmas $
             setLinkerOptions $
             disableOptimisation dflags'
        getSession
    initDynLinker env
    newHscEnvEq env

-- Set the GHC libdir to the nix libdir if it's present.
getLibdir :: IO FilePath
getLibdir = fromMaybe GHC.Paths.libdir <$> lookupEnv "NIX_GHC_LIBDIR"

-- we don't want to generate object code so we compile to bytecode
-- (HscInterpreted) which implies LinkInMemory
-- HscInterpreted
setLinkerOptions :: DynFlags -> DynFlags
setLinkerOptions df = df {
    ghcLink   = LinkInMemory
  , hscTarget = HscNothing
  , ghcMode = CompManager
  }

setIgnoreInterfacePragmas :: DynFlags -> DynFlags
setIgnoreInterfacePragmas df =
    gopt_set (gopt_set df Opt_IgnoreInterfacePragmas) Opt_IgnoreOptimChanges

disableOptimisation :: DynFlags -> DynFlags
disableOptimisation df = updOptLevel 0 df

setHiDir :: FilePath -> DynFlags -> DynFlags
setHiDir f d =
    -- override user settings to avoid conflicts leading to recompilation
    d { hiDir      = Just f}

getCacheDir :: [String] -> IO FilePath
getCacheDir opts = IO.getXdgDirectory IO.XdgCache (cacheDir </> opts_hash)
    where
        -- Create a unique folder per set of different GHC options, assuming that each different set of
        -- GHC options will create incompatible interface files.
        opts_hash = B.unpack $ encode $ H.finalize $ H.updates H.init (map B.pack opts)

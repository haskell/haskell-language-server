{-# LANGUAGE TemplateHaskell #-}
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
import           Data.Text                      (Text, pack)
import           Development.IDE.Core.Rules     (defineNoFile)
import           Development.IDE.Core.Service   (getIdeOptions)
import           Development.IDE.Core.Shake     (actionLogger, sendEvent, define, useNoFile_)
import           Development.IDE.GHC.Util
import           Development.IDE.Types.Location (fromNormalizedFilePath)
import           Development.IDE.Types.Options  (IdeOptions(IdeOptions, optTesting))
import           Development.Shake
import           GHC
import           GHC.Check                      (VersionCheck(..), makeGhcVersionChecker)
import           HIE.Bios
import           HIE.Bios.Cradle
import           HIE.Bios.Environment           (addCmdOpts)
import           HIE.Bios.Types
import           Linker                         (initDynLinker)
import           RuleTypes
import qualified System.Directory.Extra         as IO
import           System.FilePath.Posix          (addTrailingPathSeparator,
                                                 (</>))
import qualified Language.Haskell.LSP.Messages  as LSP
import qualified Language.Haskell.LSP.Types     as LSP
import Data.Aeson (ToJSON(toJSON))
import Development.IDE.Types.Logger (logDebug)
import Util

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

    logger <- actionLogger
    liftIO $ logDebug logger $ "Running cradle " <> pack (fromNormalizedFilePath nfp)

    -- If the path points to a directory, load the implicit cradle
    mbYaml <- doesDirectoryExist f <&> \isDir -> if isDir then Nothing else Just f
    cradle <- liftIO $ maybe (loadImplicitCradle $ addTrailingPathSeparator f) loadCradle mbYaml

    when optTesting $
        sendEvent $ notifyCradleLoaded f

    -- Avoid interrupting `getComponentOptions` since it calls external processes
    cmpOpts <- liftIO $ mask $ \_ -> getComponentOptions cradle
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

ghcVersionChecker :: IO VersionCheck
ghcVersionChecker = $$(makeGhcVersionChecker (pure <$> getLibdir))

checkGhcVersion :: IO (Maybe HscEnvEq)
checkGhcVersion = do
    res <- ghcVersionChecker
    case res of
        Failure err -> do
          putStrLn $ "Error while checking GHC version: " ++ show err
          return Nothing
        Mismatch {..} ->
          return $ Just GhcVersionMismatch {..}
        _ ->
          return Nothing

createSession :: ComponentOptions -> IO HscEnvEq
createSession (ComponentOptions theOpts _) = do
    libdir <- getLibdir

    cacheDir <- getCacheDir theOpts

    runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        (dflags', _targets) <- addCmdOpts theOpts dflags
        setupDynFlags cacheDir dflags'
        versionMismatch <- liftIO checkGhcVersion
        case versionMismatch of
            Just mismatch -> return mismatch
            Nothing ->  do
                env <- getSession
                liftIO $ initDynLinker env
                liftIO $ newHscEnvEq env

getCacheDir :: [String] -> IO FilePath
getCacheDir opts = IO.getXdgDirectory IO.XdgCache (cacheDir </> opts_hash)
    where
        -- Create a unique folder per set of different GHC options, assuming that each different set of
        -- GHC options will create incompatible interface files.
        opts_hash = B.unpack $ encode $ H.finalize $ H.updates H.init (map B.pack opts)

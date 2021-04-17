{-# LANGUAGE PatternSynonyms #-}

module Development.IDE.Graph(
    shake,
    shakeOptions,
    Rules, action, withoutActions, alternatives, priority, versioned,
    Action, traced,
    liftIO, actionOnException, actionFinally, actionBracket, actionCatch, actionRetry, runAfter,
    ShakeException(..),
    -- * Configuration
    ShakeOptions(..), Rebuild(..), Lint(..), Change(..),
    getShakeOptions, getShakeOptionsRules, getHashedShakeVersion,
    getShakeExtra, getShakeExtraRules, addShakeExtra,
    -- ** Command line
    shakeArgs, shakeArgsWith, shakeArgsOptionsWith, shakeOptDescrs, addHelpSuffix,
    -- ** Targets
    getTargets, addTarget, withTargetDocs, withoutTargets,
    -- ** Progress reporting
    Progress(..), progressSimple, progressDisplay, progressTitlebar, progressProgram, getProgress,
    -- ** Verbosity
    Verbosity(..), getVerbosity, putVerbose, putInfo, putWarn, putError, withVerbosity, quietly,
    -- * Running commands
    command, command_, cmd, cmd_, unit,
    Stdout(..), StdoutTrim(..), Stderr(..), Stdouterr(..), Exit(..), Process(..), CmdTime(..), CmdLine(..), FSATrace(..),
    CmdResult, CmdString, CmdOption(..),
    addPath, addEnv,
    -- * Explicit parallelism
    parallel, forP, par,
    -- * Utility functions
    copyFile', copyFileChanged,
    readFile', readFileLines,
    writeFile', writeFileLines, writeFileChanged,
    removeFiles, removeFilesAfter,
    withTempFile, withTempDir,
    withTempFileWithin, withTempDirWithin,
    -- * File rules
    need, want, (%>), (|%>), (?>), phony, (~>), phonys,
    (&%>), (&?>),
    orderOnly, orderOnlyAction,
    FilePattern, (?==), (<//>), filePattern,
    needed, trackRead, trackWrite, trackAllow,
    -- * Directory rules
    doesFileExist, doesDirectoryExist, getDirectoryContents, getDirectoryFiles, getDirectoryDirs,
    getDirectoryFilesIO,
    -- * Environment rules
    getEnv, getEnvWithDefault, getEnvError,
    -- * Oracle rules
    ShakeValue, RuleResult, addOracle, addOracleCache, addOracleHash, askOracle, askOracles,
    -- * Special rules
    alwaysRerun,
    -- * Resources
    Resource, newResource, newResourceIO, withResource, withResources,
    newThrottle, newThrottleIO,
    unsafeExtraThread,
    -- * Cache
    newCache, newCacheIO,
    historyDisable, produces,
    -- * Batching
    needHasChanged,
    resultHasChanged,
    batch,
    reschedule,
    -- * Deprecated
    askOracleWith,
    deprioritize,
    pattern Quiet, pattern Normal, pattern Loud, pattern Chatty,
    putLoud, putNormal, putQuiet
    ) where

import Development.Shake

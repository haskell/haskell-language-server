{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ShakeRestartTests (tests) where

import           Control.Concurrent.STM
import           Data.IORef
import           Data.IORef.Extra           (atomicModifyIORef'_)
import           Development.IDE.Core.Shake
import           Development.IDE.Graph      (newKey)
import           Language.LSP.VFS
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "shake restart merging"
    [ testCase "newestVFSModified" $ do
        let vfs1 = VFSModified (VFS mempty)
        newestVFSModified VFSUnmodified VFSUnmodified @?= VFSUnmodified
        newestVFSModified vfs1 VFSUnmodified @?= vfs1
        newestVFSModified VFSUnmodified vfs1 @?= vfs1

    , testCase "mergePendingRestart Nothing" $ do
        let p = PendingRestart VFSUnmodified [] ["reason"] [] []
        if mergePendingRestart p Nothing == p
          then pure ()
          else assertFailure "merging with nothing should get new"

    , testCase "mergePendingRestart Just" $ do
        done1 <- newEmptyTMVarIO
        done2 <- newEmptyTMVarIO
        let key1 = newKey ("1" :: String)
            key2 = newKey ("2" :: String)
            p1 = PendingRestart VFSUnmodified [pure [key1]] ["r1"] [] [done1]
            p2 = PendingRestart VFSUnmodified [pure [key2]] ["r2"] [] [done2]
            merged = mergePendingRestart p1 (Just p2)

        pendingRestartReasons merged @?= ["r1", "r2"]
        keys <- sequence $ pendingRestartActionBetweenSessions merged
        concat keys @?= [key2, key1]

    , testCase "RestartSlot coalescing" $ do
        slot <- newRestartSlot
        let p1 = PendingRestart VFSUnmodified [] ["r1"] [] []
            p2 = PendingRestart VFSUnmodified [] ["r2"] [] []

        atomicModifyIORef'_ (queuedRestart slot) $ Just . mergePendingRestart p1
        atomicModifyIORef'_ (queuedRestart slot) $ Just . mergePendingRestart p2

        res <- atomicModifyIORef' (queuedRestart slot) (Nothing,)
        case res of
            Nothing -> assertFailure "Should have a pending restart"
            Just p  -> pendingRestartReasons p @?= ["r2", "r1"]
    ]

instance Eq VFSModified where
    VFSUnmodified == VFSUnmodified             = True
    VFSModified (VFS _) == VFSModified (VFS _) = True
    _ == _                                     = False

instance Eq PendingRestart where
    p1 == p2 = pendingRestartVFS p1 == pendingRestartVFS p2 &&
               pendingRestartReasons p1 == pendingRestartReasons p2

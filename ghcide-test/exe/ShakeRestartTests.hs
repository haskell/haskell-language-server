{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ShakeRestartTests (tests) where

import           Control.Concurrent.STM
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

    , testCase "<>" $ do
        done1 <- newEmptyTMVarIO
        done2 <- newEmptyTMVarIO
        let key1 = newKey ("1" :: String)
            key2 = newKey ("2" :: String)
            p1 = PendingRestart VFSUnmodified [pure [key1]] ["r1"] [] [done1]
            p2 = PendingRestart VFSUnmodified [pure [key2]] ["r2"] [] [done2]
            merged = p1 <> p2

        pendingRestartReasons merged @?= ["r1", "r2"]
        keys <- sequence $ reverse $ pendingRestartActionBetweenSessions merged
        concat keys @?= [key2, key1]
    ]

instance Eq VFSModified where
    VFSUnmodified == VFSUnmodified             = True
    VFSModified (VFS _) == VFSModified (VFS _) = True
    _ == _                                     = False

instance Eq PendingRestart where
    p1 == p2 = pendingRestartVFS p1 == pendingRestartVFS p2 &&
               pendingRestartReasons p1 == pendingRestartReasons p2

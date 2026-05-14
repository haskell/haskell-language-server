{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ShakeRestartTests (tests) where

import qualified Data.Map.Lazy               as Map
import           Development.IDE.Core.Shake
import           Language.LSP.Protocol.Types (Uri (..), toNormalizedUri)
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

    , testCase "<> appends reasons in chronological order" $ do
        let p1 = PendingRestart VFSUnmodified mempty ["r1"] [] []
            p2 = PendingRestart VFSUnmodified mempty ["r2"] [] []
        pendingRestartReasons (p1 <> p2) @?= ["r1", "r2"]

    , testCase "<> takes VFS from the right operand" $ do
        let olderUri  = toNormalizedUri (Uri "older")
            newerUri  = toNormalizedUri (Uri "newer")
            unforced  = error "VFS payload should not be forced by Map.keys"
            olderVfs  = VFSModified (VFS (Map.singleton olderUri unforced))
            newerVfs  = VFSModified (VFS (Map.singleton newerUri unforced))
            older     = PendingRestart olderVfs mempty ["older"] [] []
            newer     = PendingRestart newerVfs mempty ["newer"] [] []
        case pendingRestartVFS (older <> newer) of
            VFSModified (VFS m) -> Map.keys m @?= [newerUri]
            VFSUnmodified       -> assertFailure "expected VFSModified"
    ]

instance Eq VFSModified where
    VFSUnmodified == VFSUnmodified             = True
    VFSModified (VFS _) == VFSModified (VFS _) = True
    _ == _                                     = False

instance Eq PendingRestart where
    p1 == p2 = pendingRestartVFS p1 == pendingRestartVFS p2 &&
               pendingRestartReasons p1 == pendingRestartReasons p2

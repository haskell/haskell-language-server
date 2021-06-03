{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens
import           Ide.Plugin.CallHierarchy
import qualified Language.LSP.Test        as Test
import qualified Language.LSP.Types.Lens  as L
import           System.FilePath
import           Test.Hls

plugin :: PluginDescriptor IdeState
plugin = descriptor "callHierarchy"

main :: IO ()
main = defaultTestRunner $ testGroup "Call Hierarchy" [prepareCallHierarchyTests]


prepareCallHierarchyTests :: TestTree
prepareCallHierarchyTests =
  testGroup
  "Prepare Call Hierarchy"
  [ testCase "simple call hierarchy" $ do
      runSessionWithServer plugin testDataDir $ do
        doc <- openDoc "B.hs" "haskell"
        [item] <- Test.prepareCallHierarchy (mkPrepareCallHierarchyParam doc 11 0)
        let expectedRange = Range (Position 11 0) (Position 11 1)
            expected = CallHierarchyItem "a" SkFunction Nothing Nothing (doc ^. L.uri) expectedRange expectedRange Nothing
        liftIO $ item @?= expected
  , testCase "typeclass" $ do
      runSessionWithServer plugin testDataDir $ do
        doc <- openDoc "B.hs" "haskell"
        [item] <- Test.prepareCallHierarchy (mkPrepareCallHierarchyParam doc 9 0)
        let expectedRange = Range (Position 9 0) (Position 9 1)
            expected = CallHierarchyItem "g" SkFunction Nothing Nothing (doc ^. L.uri) expectedRange expectedRange Nothing
        liftIO $ item @?= expected
  ]

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

mkPrepareCallHierarchyParam :: TextDocumentIdentifier -> Int -> Int -> CallHierarchyPrepareParams
mkPrepareCallHierarchyParam doc x y = CallHierarchyPrepareParams doc (Position x y) Nothing

mkIncomingCallsParam :: CallHierarchyItem -> CallHierarchyIncomingCallsParams
mkIncomingCallsParam = CallHierarchyIncomingCallsParams Nothing Nothing

mkOutgoingCallsParam :: CallHierarchyItem -> CallHierarchyOutgoingCallsParams
mkOutgoingCallsParam = CallHierarchyOutgoingCallsParams Nothing Nothing

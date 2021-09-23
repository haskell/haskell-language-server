module Reference (tests) where

import           Control.Lens
import           Data.Coerce
import           Data.List
import           Language.LSP.Types.Lens
import           Test.Hls
import           Test.Hls.Command
import           Test.Hspec.Expectations (shouldNotContain)

tests :: TestTree
tests =
  testGroup
    "references"
    [ testCase "works with definitions" $
        runSession hlsCommand fullCaps "test/testdata" $ do
          doc <- openDoc "References.hs" "haskell"
          let pos = Position 2 7 -- foo = bar <--
          refs <- getReferences doc pos True
          liftIO $
            map
              (Location (doc ^. uri))
              [ mkRange 2 6 2 9,
                mkRange 4 0 4 3,
                mkRange 4 14 4 17,
                mkRange 7 7 7 10,
                mkRange 8 11 8 14
              ]
              `isInfixOf` coerce refs
              @? "Contains references",
      expectFailBecause "includeDeclaration parameter ignored, always included" $
      testCase "works without definitions" $
        runSession hlsCommand fullCaps "test/testdata" $ do
          doc <- openDoc "References.hs" "haskell"
          let pos = Position 2 7 -- foo = bar <--
          refs <- getReferences doc pos False
          liftIO $ coerce refs `shouldNotContain` [Location (doc ^. uri) (mkRange 4 0 4 3)]
    ]

module Reference (tests) where

import           Control.Lens
import           Data.Coerce
import           Data.List
import           Language.LSP.Types.Lens
import           Test.Hls
import           Test.Hls.Command

tests :: TestTree
tests = testGroup "references" [
    ignoreTestBecause "Broken" $ testCase "works with definitions" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "References.hs" "haskell"
        let pos = Position 2 7 -- foo = bar <--
        refs <- getReferences doc pos True
        liftIO $ map (Location (doc ^. uri)) [
                mkRange 4 0 4 3
            , mkRange 8 11 8 14
            , mkRange 7 7 7 10
            , mkRange 4 14 4 17
            , mkRange 4 0 4 3
            , mkRange 2 6 2 9
            ] `isInfixOf` coerce refs @? "Contains references"
    -- TODO: Respect withDeclaration parameter
    -- ignoreTestBecause "Broken" $ testCase "works without definitions" $ runSession hlsCommand fullCaps "test/testdata" $ do
    --   doc <- openDoc "References.hs" "haskell"
    --   let pos = Position 2 7 -- foo = bar <--
    --   refs <- getReferences doc pos False
    --   liftIO $ refs `shouldNotContain` [Location (doc ^. uri) (mkRange 4 0 4 3)]
    ]
    where mkRange sl sc el ec = Range (Position sl sc) (Position el ec)

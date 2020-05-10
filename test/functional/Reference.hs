module Reference (tests) where

import Control.Lens
import Control.Monad.IO.Class
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Lens
import Test.HIE
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Expectations

tests :: TestTree
tests = testGroup "references" [
    testCase "works with definitions" $ runSession hieCommand fullCaps "test/testdata" $ do
        doc <- openDoc "References.hs" "haskell"
        let pos = Position 2 7 -- foo = bar <--
        refs <- getReferences doc pos True
        liftIO $ refs `shouldContain` map (Location (doc ^. uri)) [
                mkRange 4 0 4 3
            , mkRange 8 11 8 14
            , mkRange 7 7 7 10
            , mkRange 4 14 4 17
            , mkRange 4 0 4 3
            , mkRange 2 6 2 9
            ]
    -- TODO: Respect withDeclaration parameter
    -- testCase "works without definitions" $ runSession hieCommand fullCaps "test/testdata" $ do
    --   doc <- openDoc "References.hs" "haskell"
    --   let pos = Position 2 7 -- foo = bar <--
    --   refs <- getReferences doc pos False
    --   liftIO $ refs `shouldNotContain` [Location (doc ^. uri) (mkRange 4 0 4 3)]
    ]
    where mkRange sl sc el ec = Range (Position sl sc) (Position el ec)

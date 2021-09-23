module Definition (tests) where

import           Control.Lens
import           Language.LSP.Types.Lens
import           Test.Hls
import           Test.Hls.Command

tests :: TestTree
tests =
  testGroup
    "definitions"
    [ testCase "goto definition within the same file" $
        runSession hlsCommand fullCaps "test/testdata/definition" $ do
          doc <- openDoc "Foo.hs" "haskell"
          let pathToFoo = doc ^. uri
          let callToF2 = Position 4 5
          let definitionOfF2 = Location pathToFoo (mkRange 6 0 6 2)
          defs <- getDefinitions doc callToF2
          liftIO $ defs @?= InL [definitionOfF2]
    ]

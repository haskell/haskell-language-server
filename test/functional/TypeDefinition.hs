module TypeDefinition (tests) where

import Control.Lens ((^.))
import Control.Monad.IO.Class
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types.Lens as L
import System.Directory
import System.FilePath ((</>))
import Test.Hls.Util
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "type definitions" [
    testCase "finds local definition of record variable"
        $ getTypeDefinitionTest' (11, 23) 8
    , testCase "finds local definition of newtype variable"
        $ getTypeDefinitionTest' (16, 21) 13
    , testCase "finds local definition of sum type variable"
        $ getTypeDefinitionTest' (21, 13) 18
    , knownBrokenForGhcVersions [GHC88] "Definition of sum type not found from data constructor in GHC 8.8.x" $
      testCase "finds local definition of sum type constructor"
        $ getTypeDefinitionTest' (24, 7) 18
    , testCase "finds non-local definition of type def"
        $ getTypeDefinitionTest' (30, 17) 27
    , testCase "find local definition of type def"
        $ getTypeDefinitionTest' (35, 16) 32
    , testCase "find type-definition of type def in component"
        $ getTypeDefinitionTest "src/Lib2.hs" (13, 20) "src/Lib.hs" 8
    , testCase "find definition of parameterized data type"
        $ getTypeDefinitionTest' (40, 19) 37
    ]

getTypeDefinitionTest :: String -> (Int, Int) -> String -> Int -> Assertion
getTypeDefinitionTest symbolFile symbolPosition definitionFile definitionLine =
    failIfSessionTimeout . runSession hlsCommand fullCaps "test/testdata/gototest" $ do
        doc  <- openDoc symbolFile "haskell"
        _  <- openDoc definitionFile "haskell"
        defs <- getTypeDefinitions doc $ toPos symbolPosition
        fp <- liftIO $ canonicalizePath $ "test/testdata/gototest" </> definitionFile
        liftIO $ do
            length defs == 1 @? "Expecting a list containing one location, but got: " ++ show defs
            let [def] = defs
            def ^. L.uri @?= filePathToUri fp
            def ^. L.range . L.start . L.line @?= definitionLine - 1
            def ^. L.range . L.end . L.line @?= definitionLine - 1

getTypeDefinitionTest' :: (Int, Int) -> Int -> Assertion
getTypeDefinitionTest' symbolPosition definitionLine =
    getTypeDefinitionTest "src/Lib.hs" symbolPosition "src/Lib.hs" definitionLine

--NOTE: copied from Haskell.Ide.Engine.ArtifactMap
toPos :: (Int,Int) -> Position
toPos (l,c) = Position (l-1) (c-1)

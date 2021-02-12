module TypeDefinition (tests) where

import Control.Monad.IO.Class
import Data.Tuple.Extra (first3)
import Language.LSP.Test
import Language.LSP.Types
import System.FilePath ((</>))
import Test.Hls.Util
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "type definitions" [
    testCase "finds local definition of record variable"
        $ getTypeDefinitionTest' 10 23 7 0
    , testCase "finds local definition of newtype variable"
        $ getTypeDefinitionTest' 15 21 12 0
    , testCase "finds local definition of sum type variable"
        $ getTypeDefinitionTest' 20 13 17 0
    , knownBrokenForGhcVersions [GHC88] "Definition of sum type not found from data constructor in GHC 8.8.x" $
      testCase "finds local definition of sum type constructor"
        $ getTypeDefinitionTest' 23 7 17 0
    , testCase "finds non-local definition of type def"
        $ getTypeDefinitionTest' 29 19 26 0
    , testCase "find local definition of type def"
        $ getTypeDefinitionTest' 34 16 31 0
    , testCase "find type-definition of type def in component"
        $ getTypeDefinitionTest ("src/Lib2.hs", 12, 20) [("src/Lib.hs", 7, 0)]
    , testCase "find definition of parameterized data type"
        $ getTypeDefinitionTest ("src/Lib.hs", 39, 19) [ ("src/Lib.hs", 36, 0)
                                                       , ("src/Lib.hs", 38, 0)]
    ]

definitionsPath :: FilePath
definitionsPath = "test/testdata/gototest"

getTypeDefinitionTest :: SymbolLocation -> [SymbolLocation] -> Assertion
getTypeDefinitionTest (symbolFile, symbolLine, symbolCol) definitionLocations =
    failIfSessionTimeout . runSession (hlsCommand ++ " --test") fullCaps definitionsPath $ do
        doc  <- openDoc symbolFile "haskell"
        InL defs <- getTypeDefinitions doc $ Position symbolLine symbolCol
        liftIO $ defs `expectSameLocations` map (first3 (definitionsPath </>)) definitionLocations

getTypeDefinitionTest' :: Int -> Int -> Int -> Int -> Assertion
getTypeDefinitionTest' symbolLine symbolCol definitionLine definitionCol =
    getTypeDefinitionTest ("src/Lib.hs", symbolLine, symbolCol)
                          [("src/Lib.hs", definitionLine, definitionCol)]

module CabalInfoParser (cabalInfoParserUnitTests) where

import           System.FilePath                  ((</>))
import           Test.Hls                         (TestTree, testCase,
                                                   testGroup, (@?))
import           Utils                            (testDataDir)

import qualified Data.Text.IO                     as TIO

import           Data.Either                      (isRight)
import           Ide.Plugin.Cabal.CabalInfoParser (parseCabalInfo)

cabalInfoParserUnitTests :: TestTree
cabalInfoParserUnitTests = testGroup "cabal info Parser Tests"
    [ simpleParsingWorks
    ]
    where
        simpleParsingWorks = testCase "Simple parsing works" $ do
            res <- parseCabalInfo <$> TIO.readFile (testDataDir </> "cabal-info" </> "text.cabal-info")
            isRight res @? "Failed to parse well-formed input"

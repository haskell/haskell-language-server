{-# LANGUAGE OverloadedStrings #-}

module CabalInfoParser (cabalInfoParserUnitTests) where

import           System.FilePath                  ((</>))
import           Test.Hls                         (Assertion, TestTree,
                                                   assertFailure, testCase,
                                                   testGroup, (@=?), (@?))
import           Utils                            (testDataDir)

import qualified Data.Text.IO                     as TIO

import           Data.Either                      (isRight)
import           Ide.Plugin.Cabal.CabalInfoParser (parseCabalInfo)

import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Text                        (Text)

cabalInfoParserUnitTests :: TestTree
cabalInfoParserUnitTests = testGroup "cabal info Parser Tests"
    [ simpleParsingWorks
    , simpleMultiEntryParsingWorks
    ]
    where
        simpleParsingWorks =
            testCase "Simple parsing works" $ testParserWithFile "text.cabal-info" $ \ci -> do
                Map.keys ci @=? ["text"]

        simpleMultiEntryParsingWorks =
            testCase "Simple parsing works for multiple packages" $ testParserWithFile "containers-base.cabal-info" $ \ci -> do
                Map.keys ci @=? ["base", "containers"]

testParserWithFile :: FilePath -> (Map Text (Map Text [Text]) -> Assertion) -> Assertion
testParserWithFile file f = do
    res <- parseCabalInfo <$> TIO.readFile (testDataDir </> "cabal-info" </> file)
    case res of
        Left _   -> assertFailure "Failed to parse well-formed input"
        Right ci -> f ci

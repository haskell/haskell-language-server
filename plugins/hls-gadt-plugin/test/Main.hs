{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import           Control.Monad   (void)
import           Data.Either     (rights)
import qualified Data.Text       as T
import qualified Ide.Plugin.GADT as GADT
import           System.FilePath ((</>))
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

gadtPlugin :: PluginTestDescriptor ()
gadtPlugin = mkPluginTestDescriptor' GADT.descriptor "GADT"

tests :: TestTree
tests = testGroup "GADT"
    [ runTest "range" "SimpleData" 2 0 2 1
    , runTest "SimpleData" "SimpleData" 2 0 2 10
    , runTest "SimpleNewtype" "SimpleNewtype" 2 0 2 17
    , runTest "Data" "Data" 2 0 2 36
    , runTest "Newtype" "Newtype" 2 0 2 21
    , runTest "Deriving" "Deriving" 2 0 2 56
    , runTest "Infix" "Infix" 2 0 2 35
    , runTest "Record" "Record" 2 0 5 1
    , runTest "TypeVariable" "TypeVariable"  2 0 2 32
    , runTest "DataContext" "DataContext" 2 0 2 31
    , runTest "DataContextParen" "DataContextParen" 2 0 3 6
    , runTest "Forall" "Forall" 2 0 2 44
    , runTest "ConstructorContext" "ConstructorContext" 2 0 2 38
    , runTest "Context" "Context" 2 0 4 41
    , runTest "Pragma" "Pragma" 2 0 3 29
    , onlyWorkForGhcVersions (`elem`[GHC92, GHC94]) "Single deriving has different output on ghc9.2+" $
        runTest "SingleDerivingGHC92" "SingleDerivingGHC92" 2 0 3 14
    , knownBrokenForGhcVersions [GHC92,GHC94] "Single deriving has different output on ghc9.2+" $
        runTest "SingleDeriving" "SingleDeriving" 2 0 3 14
    , onlyWorkForGhcVersions (`elem`[GHC92, GHC94]) "only ghc-9.2+ enabled GADTs pragma implicitly" $
        gadtPragmaTest "ghc-9.2 don't need to insert GADTs pragma" False
    , knownBrokenForGhcVersions [GHC92,GHC94] "ghc-9.2 has enabled GADTs pragma implicitly" $
        gadtPragmaTest "insert pragma" True
    ]

gadtPragmaTest :: TestName -> Bool -> TestTree
gadtPragmaTest title hasGADT = testCase title
    $ withCanonicalTempDir
    $ \dir -> runSessionWithServer gadtPlugin dir $ do
        doc <- createDoc "A.hs" "haskell" (T.unlines ["module A where", "data Foo = Bar"])
        _ <- waitForProgressDone
        (act:_) <- findGADTAction <$> getCodeActions doc (Range (Position 1 0) (Position 1 1))
        executeCodeAction act
        let expected = T.unlines $
                ["{-# LANGUAGE GADTs #-}" | hasGADT] ++
                ["module A where", "data Foo where", "  Bar :: Foo"]
        contents <- skipManyTill anyMessage (getDocumentEdit doc)
        liftIO $ contents @?= expected

runTest :: TestName -> FilePath -> UInt -> UInt -> UInt -> UInt -> TestTree
runTest title fp x1 y1 x2 y2 =
    goldenWithHaskellDoc gadtPlugin title testDataDir fp "expected" "hs" $ \doc -> do
        _ <- waitForProgressDone
        (act:_) <- findGADTAction <$> getCodeActions doc (Range (Position x1 y1) (Position x2 y2))
        executeCodeAction act
        void $ skipManyTill anyMessage (getDocumentEdit doc)

findGADTAction :: [a |? CodeAction] -> [CodeAction]
findGADTAction = filter isGADTCodeAction . rights . map toEither

isGADTCodeAction :: CodeAction -> Bool
isGADTCodeAction CodeAction{..} = case _kind of
    Nothing -> False
    Just kind -> case kind of
        CodeActionRefactorRewrite -> True
        _                         -> False

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import           Control.Monad   (void)
import           Data.Either     (rights)
import qualified Ide.Plugin.GADT as GADT
import           System.FilePath ((</>))
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

gadtPlugin :: PluginDescriptor IdeState
gadtPlugin = GADT.descriptor "GADT"

tests :: TestTree
tests = testGroup "GADT"
    [ runTest "range" "SimpleData" 2 0 2 1
    , runTest "SimpleData" "SimpleData" 2 0 2 10
    , runTest "SimpleNewtype" "SimpleNewtype" 2 0 2 17
    , runTest "Data" "Data" 2 0 2 36
    , runTest "Newtype" "Newtype" 2 0 2 21
    , runTest "Deriving" "Deriving" 2 0 2 56
    , runTest "Infix" "Infix" 2 0 2 39
    , runTest "Record" "Record" 2 0 5 1
    , runTest "TypeVariable" "TypeVariable"  2 0 2 32
    , runTest "DataContext" "DataContext" 2 0 2 31
    , runTest "DataContextParen" "DataContextParen" 2 0 3 6
    , runTest "Forall" "Forall" 2 0 2 44
    , runTest "ConstuctorContext" "ConstructorContext" 2 0 2 38
    , runTest "Context" "Context" 2 0 4 41
    , runTest "Pragma" "Pragma" 2 0 3 29
    ]

codeActionExistenceTest :: String -> UInt -> UInt -> UInt -> UInt -> Bool -> TestTree
codeActionExistenceTest title x1 y1 x2 y2 shouldExist =
    testCase title $ runSessionWithServer gadtPlugin testDataDir $ do
    doc <- openDoc "Data.hs" "haskell"
    acts <- findGADTAction <$> getCodeActions doc (Range (Position x1 y1) (Position x2 y2))
    liftIO $ (not . null) acts @?= shouldExist

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

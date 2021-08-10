{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Ide.Plugin.Rename as Rename
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

renamePlugin :: PluginDescriptor IdeState
renamePlugin = Rename.descriptor "rename"

tests :: TestTree
tests = testGroup "rename"
    [ goldenWithRename "function name" "FunctionName" $ \doc -> do
        rename doc (Position 3 1) "baz" -- foo :: Int -> Int
    , expectFailBecause "Declaration renames not implemented" $
      goldenWithRename "function argument" "FunctionArgument" $ \doc -> do
        rename doc (Position 3 4) "y" -- foo x = x + 1
    , expectFailBecause "Declaration renames not implemented" $
      goldenWithRename "qualified function" "QualifiedFunction" $ \doc -> do
        rename doc (Position 3 24) "baz" -- bar = FunctionArgument.foo
    , expectFailBecause "Declaration renames not implemented" $
      goldenWithRename "record field" "RecordField" $ \doc -> do
        rename doc (Position 6 9) "number" -- foo Bam {n = y} = Bam {n = y + 5, s = ""}
    , expectFailBecause "Declaration renames not implemented" $
      goldenWithRename "shadowed name" "ShadowedName" $ \doc -> do
        rename doc (Position 3 8) "y" -- x = 20
    , expectFailBecause "Declaration renames not implemented" $
      goldenWithRename "type constructor" "TypeConstructor" $ \doc -> do
        rename doc (Position 2 15) "BinaryTree" -- rotateRight :: Tree a -> Tree a
    , expectFailBecause "Declaration renames not implemented" $
      goldenWithRename "data constructor" "DataConstructor" $ \doc -> do
        rename doc (Position 0 13) "Apply" -- data Expr = Op Int Int
    , expectFailBecause "Declaration renames not implemented" $
      goldenWithRename "type variable" "TypeVariable" $ \doc -> do
        rename doc (Position 0 13) "b" -- bar :: Maybe a -> Maybe a
    , goldenWithRename "imported function" "ImportedFunction" $ \doc -> do
        rename doc (Position 0 35) "baz" -- import           FunctionArgument (foo)
    , goldenWithRename "GADT" "Gadt" $ \doc -> do
        rename doc (Position 6 35) "Expr" -- Even    :: Expression Int -> Expression Bool
    ]

goldenWithRename :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithRename title path = goldenWithHaskellDoc renamePlugin title testDataDir path "expected" "hs"

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

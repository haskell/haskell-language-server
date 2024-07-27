
module SafeTests (tests) where

import qualified Data.Text            as T
import           Development.IDE.Test (expectNoMoreDiagnostics)
import           Language.LSP.Test

import           Config
import           Test.Tasty

tests :: TestTree
tests =
  testGroup
    "SafeHaskell"
    [ -- Test for https://github.com/haskell/ghcide/issues/424
      testWithDummyPluginEmpty "load" $ do
        let sourceA =
              T.unlines
                ["{-# LANGUAGE Trustworthy #-}"
                ,"module A where"
                ,"import System.IO.Unsafe"
                ,"import System.IO ()"
                ,"trustWorthyId :: a -> a"
                ,"trustWorthyId i = unsafePerformIO $ do"
                ,"  putStrLn \"I'm safe\""
                ,"  return i"]
            sourceB =
              T.unlines
                ["{-# LANGUAGE Safe #-}"
                ,"module B where"
                ,"import A"
                ,"safeId :: a -> a"
                ,"safeId = trustWorthyId"
                ]

        _ <- createDoc "A.hs" "haskell" sourceA
        _ <- createDoc "B.hs" "haskell" sourceB
        expectNoMoreDiagnostics 1 ]

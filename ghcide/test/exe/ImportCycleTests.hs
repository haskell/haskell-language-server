
{-# LANGUAGE GADTs #-}

module ImportCycleTests (tests) where

import           Config
import           Development.IDE.Test (expectDiagnostics)
import           Language.LSP.Test
import           System.FilePath      ((</>))
import           Test.Hls
import           Test.Hls.FileSystem  (copyDir)


tests :: TestTree
tests = testGroup "ImportCycleTests"
    [testWithDummyPlugin' "Import cycle" (mkIdeTestFs [copyDir "importCycle"]) test
    ]
    where
      test :: FilePath -> Session ()
      test _ = do
        let fp = "src" </> "Lib.hs"
        let fp1 = "src" </> "Lib" </> "A.hs"
        _ <- openDoc fp "haskell"
        _ <- openDoc fp1 "haskell"
        expectDiagnostics [(fp, [])]

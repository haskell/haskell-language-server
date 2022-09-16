{-# LANGUAGE OverloadedStrings #-}
module HieBios (tests) where

import           Control.Lens            ((^.))
import           Control.Monad.IO.Class
import qualified Data.Text               as T
import qualified Language.LSP.Types.Lens as L
import           System.FilePath         ((</>))
import           Test.Hls
import           Test.Hls.Command


tests :: TestTree
tests = testGroup "hie-bios" [
    testCase "loads modules inside main-is" $ do
        writeFile (hieBiosErrorPath </> "hie.yaml") ""
        runSession hlsCommand fullCaps "test/testdata/hieBiosMainIs" $ do
            doc <- openDoc "Main.hs" "haskell"
            Just mainHoverText <- getHover doc (Position 3 1)
            let hoverContents = mainHoverText ^. L.contents
            case hoverContents of
                 (HoverContents (MarkupContent _ x)) -> do
                    liftIO $ "main :: IO ()" `T.isInfixOf` x
                            @? "found hover text for main"
                 _ -> error $ "Unexpected hover contents: " ++ show hoverContents

    , expectFailBecause "hie-bios 0.11 has poor error messages" $ testCase "reports errors in hie.yaml" $ do
        writeFile (hieBiosErrorPath </> "hie.yaml") ""
        runSession hlsCommand fullCaps hieBiosErrorPath $ do
            _ <- openDoc "Foo.hs" "haskell"
            (diag:_) <- waitForDiagnostics
            liftIO $ "Expected a cradle: key containing the preferences" `T.isInfixOf` (diag ^. L.message)
                     @? "Error reported"
    ]
    where
        hieBiosErrorPath = "test/testdata/hieBiosError"

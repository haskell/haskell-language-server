
{-# LANGUAGE OverloadedLabels #-}

module GarbageCollectionTests (tests) where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Row
import qualified Data.Set                    as Set
import qualified Data.Text                   as T
import           Development.IDE.Test        (expectCurrentDiagnostics,
                                              getStoredKeys, waitForGC,
                                              waitForTypecheck)
import           Language.LSP.Protocol.Types hiding (SemanticTokenAbsolute (..),
                                              SemanticTokenRelative (..),
                                              SemanticTokensEdit (..), mkRange)
import           Language.LSP.Test
import           System.FilePath
-- import Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUtils
import           Text.Printf                 (printf)

tests :: TestTree
tests = testGroup "garbage collection"
  [ testGroup "dirty keys"
        [ testSession' "are collected" $ \dir -> do
            liftIO $ writeFile (dir </> "hie.yaml") "cradle: {direct: {arguments: [A]}}"
            doc <- generateGarbage "A" dir
            closeDoc doc
            garbage <- waitForGC
            liftIO $ assertBool "no garbage was found" $ not $ null garbage

        , testSession' "are deleted from the state" $ \dir -> do
            liftIO $ writeFile (dir </> "hie.yaml") "cradle: {direct: {arguments: [A]}}"
            docA <- generateGarbage "A" dir
            keys0 <- getStoredKeys
            closeDoc docA
            garbage <- waitForGC
            liftIO $ assertBool "something is wrong with this test - no garbage found" $ not $ null garbage
            keys1 <- getStoredKeys
            liftIO $ assertBool "keys were not deleted from the state" (length keys1 < length keys0)

        , testSession' "are not regenerated unless needed" $ \dir -> do
            liftIO $ writeFile (dir </> "hie.yaml") "cradle: {direct: {arguments: [A.hs, B.hs]}}"
            docA <- generateGarbage "A" dir
            _docB <- generateGarbage "B" dir

            -- garbage collect A keys
            keysBeforeGC <- getStoredKeys
            closeDoc docA
            garbage <- waitForGC
            liftIO $ assertBool "something is wrong with this test - no garbage found" $ not $ null garbage
            keysAfterGC <- getStoredKeys
            liftIO $ assertBool "something is wrong with this test - keys were not deleted from the state"
                (length keysAfterGC < length keysBeforeGC)

            -- re-typecheck B and check that the keys for A have not materialized back
            _docB <- generateGarbage "B" dir
            keysB <- getStoredKeys
            let regeneratedKeys = Set.filter (not . isExpected) $
                    Set.intersection (Set.fromList garbage) (Set.fromList keysB)
            liftIO $ regeneratedKeys @?= mempty

        , testSession' "regenerate successfully" $ \dir -> do
            liftIO $ writeFile (dir </> "hie.yaml") "cradle: {direct: {arguments: [A]}}"
            docA <- generateGarbage "A" dir
            closeDoc docA
            garbage <- waitForGC
            liftIO $ assertBool "no garbage was found" $ not $ null garbage
            let edit = T.unlines
                        [ "module A where"
                        , "a :: Bool"
                        , "a = ()"
                        ]
            doc <- generateGarbage "A" dir
            changeDoc doc [TextDocumentContentChangeEvent . InR . (.==) #text $ edit]
            builds <- waitForTypecheck doc
            liftIO $ assertBool "it still builds" builds
            expectCurrentDiagnostics doc [(DiagnosticSeverity_Error, (2,4), "Couldn't match expected type")]
        ]
  ]
  where
    isExpected k = any (`T.isPrefixOf` k) ["GhcSessionIO"]

    generateGarbage :: String -> FilePath -> Session TextDocumentIdentifier
    generateGarbage modName dir = do
        let fp = modName <> ".hs"
            body = printf "module %s where" modName
        doc <- createDoc fp "haskell" (T.pack body)
        liftIO $ writeFile (dir </> fp) body
        builds <- waitForTypecheck doc
        liftIO $ assertBool "something is wrong with this test" builds
        return doc

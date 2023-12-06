{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

import           Data.Bifunctor
import           Data.ByteString             as BS
import           Data.Default
import qualified Data.Maybe
import           Data.Text
import           Development.IDE.GHC.Compat
-- import GHC.Data.StringBuffer
-- import GHC.Driver.Config.Parser
-- import GHC.Hs
-- import GHC.Hs.Dump
-- import qualified GHC.Parser as Parser ( parseModule )
-- import GHC.Parser.Lexer
-- import GHC.Platform
-- import GHC.Plugins
-- import GHC.Settings
-- import GHC.Settings.Config
-- import GHC.Generics
-- import Data.Generics (everything, mkQ, extQ)
-- import GHC
-- import qualified GHC
-- import GHC.Paths (libdir)
-- import GHC.Tc.Types
-- import GHC.Tc.Utils.Env (lookupGlobal)
import           Development.IDE
-- import Development.IDE.Test
-- import Test.Hls
-- import Test.Hls.Util              (withCanonicalTempDir)
import           Control.Arrow               (Arrow ((***)), (&&&), (+++))
import           Data.Data
import           Data.Functor                (void)
import           Data.Map                    as Map
import           Data.String                 (fromString)
import           System.Environment.Blank
import           System.FilePath
-- import qualified Development.IDE.Core.Shake           as Shake
import           Ide.Types
import qualified Test.Hls                    (PluginTestDescriptor,
                                              mkPluginTestDescriptor',
                                              runSessionWithServerInTmpDir,
                                              waitForAction)
import qualified Test.Hls.FileSystem         as FS
-- import Development.IDE.Plugin.Test
import           Control.Lens                hiding (use)
import qualified Data.List                   as List
import           Data.Maybe                  (fromJust)
import qualified Data.Set                    as Set
import           Ide.Plugin.Error            (getNormalizedFilePathE)
import           Ide.Plugin.SemanticTokens
import qualified Language.LSP.Protocol.Lens  as L
import           Language.LSP.Protocol.Types
import           Language.LSP.Protocol.Types (SemanticTokensParams (..))
import qualified Language.LSP.Test           as Test
import           Test.Hls
import           Test.Hls                    (TextDocumentIdentifier,
                                              getCodeLenses, openDoc,
                                              waitForAction)
import           Test.Hls.Util               (withCanonicalTempDir)

getUniqueName :: (NamedThing a) => a -> Name
getUniqueName = getName

-- astToString :: (Data a) => a -> String
-- astToString = showSDoc fakeDynFlags . showAstDataFull

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

mkFs :: [FS.FileTree] -> FS.VirtualFileTree
mkFs = FS.mkVirtualFileTree testDataDir

semanticTokensPlugin :: Test.Hls.PluginTestDescriptor ()
semanticTokensPlugin = Test.Hls.mkPluginTestDescriptor' Ide.Plugin.SemanticTokens.descriptor "SemanticTokens"


mkSemanticTokensParams :: TextDocumentIdentifier -> SemanticTokensParams
mkSemanticTokensParams doc = SemanticTokensParams Nothing Nothing doc

main1 :: IO ()
main1 = do

    let filePath = "./test/testdata/T1.hs"
    content <- Prelude.readFile filePath
    Test.Hls.runSessionWithServerInTmpDir def semanticTokensPlugin (mkFs $ FS.directProject "T1.hs") $ do
        doc <- openDoc "T1.hs" "haskell"
        res <- waitForAction "TypeCheck" doc
        -- res <- waitForAction "SemanticTokens.semanticTokensFull" doc
        res <- Test.getSemanticTokens doc
        case res ^? _L  of
          Just tokens -> liftIO $ mapM_ print $ recoverSemanticTokens content tokens
          _ -> liftIO $ print "error"
        liftIO $ print res
        lenses <- getCodeLenses doc
        liftIO $ print lenses

    -- let filePath = "hello.hs"
    -- content <- Prelude.readFile filePath
    -- runGhc (Just libdir) $ do
    --     dflags <- getSessionDynFlags
    --     setSessionDynFlags dflags
    --     target <- guessTarget filePath Nothing Nothing
    --     setTargets [target]
    --     load LoadAllTargets
    --     -- hsc_typecheck :: Bool -- ^ Keep renamed source?
    --     --       -> ModSummary -> Maybe HsParsedModule
    --     --       -> Hsc (TcGblEnv, RenamedStuff)
    --     modSum <- getModSummary $ mkModuleName "Main"
    --     parsedModule <- parseModule modSum
    --     typecheckedModule <- typecheckModule parsedModule
    --     let rms = fromJust $ renamedSource typecheckedModule
    --     let tcGblEnv = fst $ tm_internals_ typecheckedModule
        -- -- mkHieFileWithSource :: FilePath
        -- --             -> BS.ByteString
        -- --             -> ModSummary
        -- --             -> TcGblEnv
        -- --             -> RenamedSource -> HieFile
        -- -- let file = mkHieFileWithSource filePath content modSum tcGblEnv rms
        -- file <- mkHieFile modSum tcGblEnv rms
        -- let ast = snd $ List.head $ Map.toList $ getAsts $ hie_asts file
        -- let fileContent = srcSpanFile $ nodeSpan ast
        -- liftIO $ print fileContent
        -- let x = getter content ast
        -- -- liftIO $ mapM_ print x
        -- liftIO $ Prelude.writeFile "out.txt" $ Prelude.unlines $ fmap show x

        -- let mm = getSourcedNodeInfo ast

        -- liftIO $ Prelude.writeFile "out2.txt" $ showSDoc fakeDynFlags $ ppr ast
        -- liftIO $ Prelude.writeFile "out3.txt" $ astToString $ tm_renamed_source typecheckedModule
        -- liftIO $ Prelude.writeFile "out4.txt" $ Prelude.unlines (collectToString <$> nameGetter rms)
        -- liftIO $ Prelude.writeFile "out5.txt" $ Prelude.unlines $ fmap semanticTokenToString $ toSemanticTokens $ nameGetter rms

        -- liftIO $ print $ Map.size $ nodeIdentifiers $ snd $ List.head $ Map.toList mm

        -- liftIO $ mapM (print . (astToString &&& (show . nameUnique) &&& (show . nameSrcSpan))) names
        return ()

semanticTokensTests :: TestTree
semanticTokensTests =
  testGroup
  "get semantic Tokens"
  [ testCase "variable" $ do
        let filePath = "./test/testdata/T1.hs"
        content <- Prelude.readFile filePath
        Test.Hls.runSessionWithServerInTmpDir def semanticTokensPlugin (mkFs $ FS.directProject "T1.hs") $ do
            doc <- openDoc "T1.hs" "haskell"
            res <- waitForAction "TypeCheck" doc
            -- res <- waitForAction "SemanticTokens.semanticTokensFull" doc
            res <- Test.getSemanticTokens doc
            case res ^? _L  of
                Just tokens -> liftIO $ mapM_ print $ recoverSemanticTokens content tokens
                _ -> liftIO $ print "error"
            liftIO $ print res
            lenses <- getCodeLenses doc
            liftIO $ print lenses
  ]

main :: IO ()
main = defaultTestRunner $
  testGroup "Semantic tokens"
    [ semanticTokensTests ]

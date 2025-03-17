
module UnitTests (tests) where

import           Config                            (mkIdeTestFs)
import           Control.Concurrent
import           Control.Monad.IO.Class            (liftIO)
import           Data.IORef
import           Data.IORef.Extra                  (atomicModifyIORef_)
import           Data.List.Extra
import           Data.String                       (IsString (fromString))
import qualified Data.Text                         as T
import           Development.IDE.Core.FileStore    (getModTime)
import qualified Development.IDE.Plugin.HLS.GhcIde as Ghcide
import qualified Development.IDE.Types.Diagnostics as Diagnostics
import           Development.IDE.Types.Location
import qualified FuzzySearch
import           Ide.Logger                        (Recorder, WithPriority)
import           Ide.PluginUtils                   (pluginDescToIdePlugins)
import           Ide.Types
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types       hiding
                                                   (SemanticTokenAbsolute (..),
                                                    SemanticTokenRelative (..),
                                                    SemanticTokensEdit (..),
                                                    mkRange)
import           Language.LSP.Test
import           Network.URI
import qualified Progress
import           System.IO.Extra                   hiding (withTempDir)
import           System.Mem                        (performGC)
import           Test.Hls                          (IdeState, def,
                                                    runSessionWithServerInTmpDir,
                                                    waitForProgressDone)
import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.HUnit
import           Text.Printf                       (printf)

tests :: TestTree
tests = do
  testGroup "Unit"
     [ testCase "empty file path does NOT work with the empty String literal" $
         uriToFilePath' (fromNormalizedUri $ filePathToUri' "") @?= Just "."
     , testCase "empty file path works using toNormalizedFilePath'" $
         uriToFilePath' (fromNormalizedUri $ filePathToUri' (toNormalizedFilePath' "")) @?= Just ""
     , testCase "empty path URI" $ do
         Just URI{..} <- pure $ parseURI (T.unpack $ getUri $ fromNormalizedUri emptyPathUri)
         uriScheme @?= "file:"
         uriPath @?= ""
     , testCase "from empty path URI" $ do
         let uri = Uri "file://"
         uriToFilePath' uri @?= Just ""
     , testCase "showDiagnostics prints ranges 1-based (like vscode)" $ do
         let diag = Diagnostics.FileDiagnostic "" Diagnostics.ShowDiag Diagnostic
               {  _codeDescription = Nothing
                , _data_ = Nothing
                , _range = Range
                   { _start = Position{_line = 0, _character = 1}
                   , _end = Position{_line = 2, _character = 3}
                   }
               , _severity = Nothing
               , _code = Nothing
               , _source = Nothing
               , _message = ""
               , _relatedInformation = Nothing
               , _tags = Nothing
               } Diagnostics.NoStructuredMessage
         let shown = T.unpack (Diagnostics.showDiagnostics [diag])
         let expected = "1:2-3:4"
         assertBool (unwords ["expected to find range", expected, "in diagnostic", shown]) $
             expected `isInfixOf` shown
     , testCase "notification handlers run in priority order" $ do
        orderRef <- newIORef []
        let
            plugins ::Recorder (WithPriority Ghcide.Log) -> IdePlugins IdeState
            plugins recorder = pluginDescToIdePlugins $
                [ (priorityPluginDescriptor i)
                    { pluginNotificationHandlers = mconcat
                        [ mkPluginNotificationHandler SMethod_TextDocumentDidOpen $ \_ _ _ _ ->
                            liftIO $ atomicModifyIORef_ orderRef (i:)
                        ]
                    }
                    | i <- [1..20]
                ] ++ Ghcide.descriptors recorder
            priorityPluginDescriptor i = (defaultPluginDescriptor (fromString $ show i) ""){pluginPriority = i}

        runSessionWithServerInTmpDir def plugins (mkIdeTestFs []) $ do
            _ <- createDoc "A.hs" "haskell" "module A where"
            waitForProgressDone
            actualOrder <- liftIO $ reverse <$> readIORef orderRef

            -- Handlers are run in priority descending order
            liftIO $ actualOrder @?= [20, 19 .. 1]
     , ignoreTestBecause "The test fails sometimes showing 10000us" $
         testCase "timestamps have millisecond resolution" $ do
           resolution_us <- findResolution_us 1
           let msg = printf "Timestamps do not have millisecond resolution: %dus" resolution_us
           assertBool msg (resolution_us <= 1000)
     , Progress.tests
     , FuzzySearch.tests
     ]

findResolution_us :: Int -> IO Int
findResolution_us delay_us | delay_us >= 1000000 = error "Unable to compute timestamp resolution"
findResolution_us delay_us = withTempFile $ \f -> withTempFile $ \f' -> do
    performGC
    writeFile f ""
    threadDelay delay_us
    writeFile f' ""
    t <- getModTime f
    t' <- getModTime f'
    if t /= t' then return delay_us else findResolution_us (delay_us * 10)

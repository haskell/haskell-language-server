module BootTests (tests) where

import           Config                          (checkDefs, mkR, runInDir,
                                                  runWithExtraFiles)
import           Control.Applicative.Combinators
import           Control.Monad
import           Control.Monad.IO.Class          (liftIO)
import           Development.IDE.GHC.Util
import           Development.IDE.Test            (expectNoMoreDiagnostics,
                                                  isReferenceReady)
import           Development.IDE.Types.Location
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types     hiding
                                                 (SemanticTokenAbsolute (..),
                                                  SemanticTokenRelative (..),
                                                  SemanticTokensEdit (..),
                                                  mkRange)
import           Language.LSP.Test
import           System.FilePath                 ((</>))
import           Test.Tasty
import           Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "boot"
  [ testCase "boot-def-test" $ runWithExtraFiles "boot" $ \dir -> do
        let cPath = dir </> "C.hs"
        cSource <- liftIO $ readFileUtf8 cPath
        -- Dirty the cache
        liftIO $ runInDir dir $ do
            cDoc <- createDoc cPath "haskell" cSource
            -- We send a hover request then wait for either the hover response or
            -- `ghcide/reference/ready` notification.
            -- Once we receive one of the above, we wait for the other that we
            -- haven't received yet.
            -- If we don't wait for the `ready` notification it is possible
            -- that the `getDefinitions` request/response in the outer ghcide
            -- session will find no definitions.
            let hoverParams = HoverParams cDoc (Position 4 3) Nothing
            hoverRequestId <- sendRequest SMethod_TextDocumentHover hoverParams
            let parseReadyMessage = isReferenceReady cPath
            let parseHoverResponse = responseForId SMethod_TextDocumentHover hoverRequestId
            hoverResponseOrReadyMessage <- skipManyTill anyMessage ((Left <$> parseHoverResponse) <|> (Right <$> parseReadyMessage))
            _ <- skipManyTill anyMessage $
              case hoverResponseOrReadyMessage of
                Left _  -> void parseReadyMessage
                Right _ -> void parseHoverResponse
            closeDoc cDoc
        cdoc <- createDoc cPath "haskell" cSource
        locs <- getDefinitions cdoc (Position 7 4)
        let floc = mkR 9 0 9 1
        checkDefs locs (pure [floc])
  , testCase "graph with boot modules" $ runWithExtraFiles "boot2" $ \dir -> do
      _ <- openDoc (dir </> "A.hs") "haskell"
      expectNoMoreDiagnostics 2
  ]

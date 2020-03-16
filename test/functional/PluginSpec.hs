{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module PluginSpec where

import           Control.Applicative.Combinators
import           Control.Lens hiding (List)
-- import           Control.Monad
import           Control.Monad.IO.Class
-- import           Data.Aeson
-- import           Data.Default
-- import qualified Data.HashMap.Strict as HM
-- import           Data.Maybe
-- import qualified Data.Text as T
-- import Language.Haskell.LSP.Test
import           Language.Haskell.LSP.Test as Test
import           Language.Haskell.LSP.Types
-- import qualified Language.Haskell.LSP.Types.Capabilities as C
import qualified Language.Haskell.LSP.Types.Lens as L
import           Test.Hspec
import           TestUtils

#if __GLASGOW_HASKELL__ < 808
-- import           Data.Monoid ((<>))
#endif

-- ---------------------------------------------------------------------

spec :: Spec
spec = do
  describe "composes code actions" $
    it "provides 3.8 code actions" $ runSession hieCommandExamplePlugin fullCaps "test/testdata" $ do

      doc <- openDoc "Format.hs" "haskell"
      _diags@(diag1:_) <- waitForDiagnostics

      -- liftIO $ putStrLn $ "diags = " ++ show diags -- AZ
      liftIO $ do
        -- length diags `shouldBe` 1
        diag1 ^. L.range `shouldBe` Range (Position 0 0) (Position 1 0)
        diag1 ^. L.severity `shouldBe` Just DsError
        diag1 ^. L.code `shouldBe` Nothing
        -- diag1 ^. L.source `shouldBe` Just "example2"

        -- diag2 ^. L.source `shouldBe` Just "example"

      _cas@(CACodeAction ca:_) <- getAllCodeActions doc
      -- liftIO $ length cas `shouldBe` 2

      -- liftIO $ putStrLn $ "cas = " ++ show cas -- AZ

      liftIO $ [ca ^. L.title] `shouldContain` ["Add TODO Item 1"]

      liftIO $ putStrLn $ "A" -- AZ
      executeCodeAction ca
      liftIO $ putStrLn $ "B" -- AZ

      -- _ <- skipMany (message @RegisterCapabilityRequest)
      -- liftIO $ putStrLn $ "B2" -- AZ

      _diags2 <- waitForDiagnostics
      -- liftIO $ putStrLn $ "diags2 = " ++ show _diags2 -- AZ

      -- contents <- getDocumentEdit doc
      liftIO $ putStrLn $ "C" -- AZ
      -- liftIO $ contents `shouldBe` "main = undefined\nfoo x = x\n"

      -- noDiagnostics
      return ()

  describe "symbol providers" $
    it "combines symbol providers" $ runSession hieCommandExamplePlugin fullCaps "test/testdata" $ do

      doc <- openDoc "Format.hs" "haskell"

      _ <- waitForDiagnostics

      id2 <- sendRequest TextDocumentDocumentSymbol (DocumentSymbolParams doc Nothing)
      symbolsRsp <- skipManyTill anyNotification message :: Session DocumentSymbolsResponse
      liftIO $ symbolsRsp ^. L.id `shouldBe` responseId id2

      liftIO $ symbolsRsp ^. L.result `shouldBe`
         Just (DSDocumentSymbols
               (List [DocumentSymbol
                        "Example_symbol_name"
                        Nothing
                        SkVariable
                        Nothing
                        (Range {_start = Position {_line = 2, _character = 0}
                                       , _end = Position {_line = 2, _character = 5}})
                        (Range {_start = Position {_line = 2, _character = 0}
                                                , _end = Position {_line = 2, _character = 5}})
                      Nothing
                     ,DocumentSymbol "Example2_symbol_name"
                                     Nothing
                                     SkVariable
                                     Nothing
                                     (Range {_start = Position {_line = 4, _character = 1}
                                                      , _end = Position {_line = 4, _character = 7}})
                                     (Range {_start = Position {_line = 4, _character = 1}
                                                               , _end = Position {_line = 4, _character = 7}})
                                     Nothing]))

      return ()

module CPPTests (tests) where

import           Control.Exception           (catch)
import qualified Data.Text                   as T
import           Development.IDE.Test        (Cursor, expectDiagnostics,
                                              expectNoMoreDiagnostics)
import           Language.LSP.Protocol.Types hiding (SemanticTokenAbsolute (..),
                                              SemanticTokenRelative (..),
                                              SemanticTokensEdit (..), mkRange)
import           Language.LSP.Test
-- import Test.QuickCheck.Instances ()
import           Config
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup "cpp"
    [ testCase "cpp-error" $ do
        let content =
              T.unlines
                [ "{-# LANGUAGE CPP #-}",
                  "module Testing where",
                  "#ifdef FOO",
                  "foo = 42"
                ]
        -- The error locations differ depending on which C-preprocessor is used.
        -- Some give the column number and others don't (hence maxBound == -1 unsigned). Assert either
        -- of them.
        (run $ expectError content (2, maxBound))
          `catch` ( \e -> do
                      let _ = e :: HUnitFailure
                      run $ expectError content (2, 1)
                  )
    , testWithDummyPluginEmpty "cpp-ghcide" $ do
        _ <- createDoc "A.hs" "haskell" $ T.unlines
          ["{-# LANGUAGE CPP #-}"
          ,"main ="
          ,"#ifdef __GHCIDE__"
          ,"  worked"
          ,"#else"
          ,"  failed"
          ,"#endif"
          ]
        expectDiagnostics [("A.hs", [(DiagnosticSeverity_Error, (3, 2), "Variable not in scope: worked")])]
    ]
  where
    expectError :: T.Text -> Cursor -> Session ()
    expectError content cursor = do
      _ <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics
        [ ( "Testing.hs",
            [(DiagnosticSeverity_Error, cursor, "error: unterminated")]
          )
        ]
      expectNoMoreDiagnostics 0.5

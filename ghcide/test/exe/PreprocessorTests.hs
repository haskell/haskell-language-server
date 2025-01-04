
module PreprocessorTests (tests) where

import qualified Data.Text                   as T
import           Development.IDE.Test        (expectDiagnostics)
import           Language.LSP.Protocol.Types hiding (SemanticTokenAbsolute (..),
                                              SemanticTokenRelative (..),
                                              SemanticTokensEdit (..), mkRange)
import           Language.LSP.Test
-- import Test.QuickCheck.Instances ()
import           Config
import           Test.Tasty

tests :: TestTree
tests = testWithDummyPluginEmpty "preprocessor" $ do
  let content =
        T.unlines
          [ "{-# OPTIONS_GHC -F -pgmF=ghcide-test-preprocessor #-}"
          , "module Testing where"
          , "y = x + z" -- plugin replaces x with y, making this have only one diagnostic
          ]
  _ <- createDoc "Testing.hs" "haskell" content
  expectDiagnostics
    [ ( "Testing.hs",
        [(DiagnosticSeverity_Error, (2, 8), "Variable not in scope: z", Nothing)] -- TODO: Why doesn't this work with expected code "GHC-88464"?
      )
    ]

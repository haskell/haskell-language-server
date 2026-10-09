
module SymlinkTests (tests) where

import           Control.Monad.IO.Class      (liftIO)
import           Development.IDE.Test        (expectDiagnosticsWithTags)
import           Language.LSP.Protocol.Types hiding (SemanticTokenAbsolute (..),
                                              SemanticTokenRelative (..),
                                              SemanticTokensEdit (..), mkRange)
import           Language.LSP.Test
import           System.Directory
import           System.FilePath

import           Config
import           Test.Tasty
import           Test.Tasty.HUnit

-- | Tests for projects that use symbolic links one way or another
tests :: TestTree
tests =
  testGroup "Projects using Symlinks"
    [ testCase "Module is symlinked" $ runWithExtraFiles "symlink" $ \dir -> do
        liftIO $ createFileLink (dir </> "some_loc" </> "Sym.hs") (dir </> "other_loc" </> "Sym.hs")
        let fooPath = dir </> "src" </> "Foo.hs"
        _ <- openDoc fooPath "haskell"
        expectDiagnosticsWithTags  [("src" </> "Foo.hs", [(DiagnosticSeverity_Warning, (2, 0), "The import of 'Sym' is redundant", Nothing, Just DiagnosticTag_Unnecessary)])]
        pure ()
    ]


module PluginSimpleTests (tests) where

import           Control.Monad.IO.Class      (liftIO)
import           Development.IDE.GHC.Compat  (GhcVersion (..))
import           Development.IDE.Test        (expectDiagnostics)
import           Language.LSP.Protocol.Types hiding (SemanticTokenAbsolute (..),
                                              SemanticTokenRelative (..),
                                              SemanticTokensEdit (..), mkRange)
import           Language.LSP.Test
import           System.FilePath
-- import Test.QuickCheck.Instances ()
import           Config
import           Test.Hls.Util               (EnvSpec (..), OS (..),
                                              knownBrokenForGhcVersions,
                                              knownBrokenInSpecificEnv)
import           Test.Tasty

tests :: TestTree
tests =
  -- Build profile: -w ghc-9.4.2 -O1
  -- In order, the following will be built (use -v for more details):
  -- - ghc-typelits-natnormalise-0.7.7 (lib) (requires build)
  -- - ghc-typelits-knownnat-0.7.7 (lib) (requires build)
  -- - plugin-1.0.0 (lib) (first run)
  -- Starting     ghc-typelits-natnormalise-0.7.7 (lib)
  -- Building     ghc-typelits-natnormalise-0.7.7 (lib)

  -- Failed to build ghc-typelits-natnormalise-0.7.7.
  -- Build log (
  -- C:\cabal\logs\ghc-9.4.2\ghc-typelits-_-0.7.7-3f036a52a0d9bfc3389d1852a87da2e87c6de2e4.log
  -- ):
  -- Preprocessing library for ghc-typelits-natnormalise-0.7.7..
  -- Building library for ghc-typelits-natnormalise-0.7.7..
  -- [1 of 3] Compiling GHC.TypeLits.Normalise.SOP ( src\GHC\TypeLits\Normalise\SOP.hs, dist\build\GHC\TypeLits\Normalise\SOP.o )
  -- [2 of 3] Compiling GHC.TypeLits.Normalise.Unify ( src\GHC\TypeLits\Normalise\Unify.hs, dist\build\GHC\TypeLits\Normalise\Unify.o )
  -- [3 of 3] Compiling GHC.TypeLits.Normalise ( src-ghc-9.4\GHC\TypeLits\Normalise.hs, dist\build\GHC\TypeLits\Normalise.o )
  -- C:\tools\ghc-9.4.2\lib\../mingw/bin/llvm-ar.exe: error: dist\build\objs-5156\libHSghc-typelits-_-0.7.7-3f036a52a0d9bfc3389d1852a87da2e87c6de2e4.a: No such file or directory

  -- Error: cabal: Failed to build ghc-typelits-natnormalise-0.7.7 (which is
  -- required by plugin-1.0.0). See the build log above for details.
  testWithExtraFiles "simple plugin"  "plugin-knownnat" $ \dir -> do
    _ <- openDoc (dir </> "KnownNat.hs") "haskell"
    liftIO $ writeFile (dir</>"hie.yaml")
      "cradle: {cabal: [{path: '.', component: 'lib:plugin'}]}"

    expectDiagnostics
      [ ( "KnownNat.hs",
          [(DiagnosticSeverity_Error, (9, 15), "Variable not in scope: c")]
          )
      ]


module PluginSimpleTests (tests) where

import           Config
import           Development.IDE.Test        (expectDiagnostics)
import           Language.LSP.Protocol.Types hiding (SemanticTokenAbsolute (..),
                                              SemanticTokenRelative (..),
                                              SemanticTokensEdit (..), mkRange)
import           Language.LSP.Test
import           System.FilePath
import           Test.Hls.FileSystem
import           Test.Tasty
import qualified Test.Hls.FileSystem as FS

tests :: TestTree
tests = testGroup "ghc-plugins"
  [
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
    testWithDummyPlugin "simple plugin" pluginKnownNatVfs $ do
      _ <- openDoc "KnownNat.hs" "haskell"

      expectDiagnostics
        [ ( "KnownNat.hs",
            [(DiagnosticSeverity_Error, (9, 15), "Variable not in scope: c", Just "GHC-88464")]
            )
        ]
  , testWithDummyPlugin "simple parser plugin" pluginParsreVfs $ do
      _ <- openDoc "usage/File1.hs" "haskell"

      expectDiagnostics
        [ ( ("usage" </> "File1.hs"),
            [(DiagnosticSeverity_Warning, (5, 0), "Top-level binding with no type signature: bar :: Int", Just "GHC-38417")]
            )
        ]
  ]

pluginKnownNatVfs :: VirtualFileTree
pluginKnownNatVfs = FS.mkVirtualFileTree ("ghcide-test" </> "data" </> "plugin-knownnat") $
  FS.simpleCabalProject
    [ "cabal.project"
    , "KnownNat.hs"
    , "plugin.cabal"
    ]

pluginParsreVfs :: VirtualFileTree
pluginParsreVfs = FS.mkVirtualFileTree ("ghcide-test" </> "data" </> "plugin-parser") $
  [ simpleCabalCradle
  , copy "cabal.project"
  , directory "plugin"
    [ copy "plugin/Plugin.hs"
    , copy "plugin/plugin.cabal"
    ]
  , directory "usage"
    [ copy "usage/File1.hs"
    , copy "usage/File2.hs"
    , copy "usage/usage.cabal"
    ]
  ]


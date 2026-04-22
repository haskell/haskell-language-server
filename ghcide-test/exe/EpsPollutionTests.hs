-- | Regression test: edits that break typechecking can leave HLS's
-- shared 'ExternalPackageState' ('EPS') polluted with interfaces and
-- instances from /home-package/ modules. The next successful typecheck
-- of a module that also legitimately has those home modules in its HPT
-- reports \"Overlapping instance\" with both matches pointing at the
-- same source location, because 'tcGetInstEnvs' returns the same
-- 'ClsInst' twice (once via @ie_global@ from the EPS, once via
-- @ie_local@ from 'hptInstancesBelow').
--
-- The pollution enters through 'Development.IDE.Spans.Documentation.mkDocMap'.
-- Its 'Rules.GetDocMap' rule reads three inputs via independent
-- @useWithStale_@ calls: 'TypeCheck', 'GhcSessionDeps' and 'GetHieAst'.
-- These three can diverge: an edit that merely changes imports lets
-- 'GhcSessionDeps' re-evaluate (fresh, with a different HPT) while
-- 'TypeCheck' and 'GetHieAst' fall back to their last-successful values.
-- If the stale 'RefMap' references a name whose module is no longer in
-- the fresh HPT, 'mkDocMap' asks 'getDocsBatch' for its docs;
-- 'loadSysInterface' does not find the module in the HUG and calls
-- 'loadInterface', which puts the home-module interface -- /with its
-- instance environment/ -- into the shared EPS @IORef@. The EPS never
-- evicts anything, so the pollution is permanent for the session.
module EpsPollutionTests (tests) where

import           Config                      (runWithExtraFiles)
import           Control.Lens                ((^.))
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.Text                   as T
import           Development.IDE.GHC.Util    (readFileUtf8)
import           Development.IDE.Test        (waitForTypecheck)
import qualified Language.LSP.Protocol.Lens  as L
import           Language.LSP.Protocol.Types
import           Language.LSP.Test
import           System.FilePath
import           Test.Hls                    (expectFailBecause,
                                              waitForDiagnosticsFrom)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "eps-pollution"
    [ expectFailBecause
        "The EPS gets polluted with a home-module instance via GetDocMap's \
        \inconsistent stale-value snapshot; the next successful typecheck \
        \sees the instance twice."
        staleHieProvokesOverlapping
    ]

-- The fixture at ghcide-test/data/multi-unit-eps-pollution/ sets up two
-- home units: unit @a@ provides module @A@ which defines @MyClass@ and
-- @instance MyClass AType@; unit @c@ provides module @C@ which imports
-- @A@ and uses @myMethod@ on an @AType@ value (forcing instance
-- resolution).

staleHieProvokesOverlapping :: TestTree
staleHieProvokesOverlapping =
  testCase "Stale RefMap must not provoke overlapping-instance error" $
  runWithExtraFiles "multi-unit-eps-pollution" $ \dir -> do
    let cPath = dir </> "c" </> "C.hs"
    originalC <- liftIO $ readFileUtf8 cPath
    let brokenC = T.replace "import A" "" originalC
    cdoc <- openDoc cPath "haskell"
    True <- waitForTypecheck cdoc
    -- Hovering triggers the hover pipeline, which forces GetDocMap.
    -- While C is healthy this populates GetHieAst with a RefMap
    -- referencing A's names -- the stale value we rely on below.
    _ <- getHover cdoc (hoverOnMyMethod originalC)
    -- Break C's import of A. C fails to typecheck, but GhcSessionDeps
    -- re-evaluates successfully (it only needs the import list) with an
    -- HPT that no longer contains A. A further hover forces GetDocMap
    -- to run with the fresh GhcSessionDeps alongside the stale RefMap;
    -- loadSysInterface(A) then runs and pollutes the EPS.
    changeDoc cdoc [TextDocumentContentChangeEvent . InR .
      TextDocumentContentChangeWholeDocument $ brokenC]

    void $ waitForDiagnosticsFrom cdoc
    void $ getHover cdoc (hoverOnMyMethod brokenC)

    -- Repair C. The next typecheck legitimately has A in its HPT; with
    -- the polluted EPS it also has A's ClsInst in eps_inst_env, so
    -- instance resolution for 'myMethod x :: AType -> String' finds
    -- two matches with identical source locations.
    changeDoc cdoc [TextDocumentContentChangeEvent . InR .
      TextDocumentContentChangeWholeDocument $ originalC]
    diags <- waitForDiagnosticsFrom cdoc
    liftIO $ assertBool
      ("Expected no overlapping-instance errors, got diagnostics:\n"
        ++ unlines (map (T.unpack . (^. L.message)) diags))
      (not (any isOverlappingInstance diags))
  where
    isOverlappingInstance d =
      "Overlapping instance" `T.isInfixOf` (d ^. L.message)

-- | 'Position' at the first occurrence of @myMethod@ in the given source.
--   Computed rather than hard-coded because the broken variant has one
--   fewer line than the original.
hoverOnMyMethod :: T.Text -> Position
hoverOnMyMethod src =
    case [ Position row (fromIntegral (T.length prefix))
         | (row, line) <- zip [0..] (T.lines src)
         , let (prefix, rest) = T.breakOn "myMethod" line
         , not (T.null rest)
         ] of
      p : _ -> p
      []    -> error "hoverOnMyMethod: no occurrence of 'myMethod'"

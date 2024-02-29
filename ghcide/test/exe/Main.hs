-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-
 NOTE On enforcing determinism

   The tests below use two mechanisms to enforce deterministic LSP sequences:

    1. Progress reporting: waitForProgress(Begin|Done)
    2. Diagnostics: expectDiagnostics

    Either is fine, but diagnostics are generally more reliable.

    Mixing them both in the same test is NOT FINE as it will introduce race
    conditions since multiple interleavings are possible. In other words,
    the sequence of diagnostics and progress reports is not deterministic.
    For example:

    < do something >
    waitForProgressDone
    expectDiagnostics [...]

    - When the diagnostics arrive after the progress done message, as they usually do, the test will pass
    - When the diagnostics arrive before the progress done msg, when on a slow machine occasionally, the test will timeout

    Therefore, avoid mixing both progress reports and diagnostics in the same test
 -}



module Main (main) where
-- import Test.QuickCheck.Instances ()
import           Data.Function                            ((&))
import           Ide.Logger             (Logger (Logger),
                                                           LoggingColumn (DataColumn, PriorityColumn),
                                                           Pretty (pretty),
                                                           Priority (Debug),
                                                           Recorder (Recorder, logger_),
                                                           WithPriority (WithPriority, priority),
                                                           cfilter,
                                                           cmapWithPrio,
                                                           makeDefaultStderrRecorder)
import           GHC.Stack                                (emptyCallStack)
import qualified HieDbRetry
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun

import LogType ()
import OpenCloseTest
import InitializeResponseTests
import CompletionTests
import CPPTests
import DiagnosticTests
import CodeLensTests
import OutlineTests
import HighlightTests
import FindDefinitionAndHoverTests
import PluginSimpleTests
import PreprocessorTests
import THTests
import SymlinkTests
import SafeTests
import UnitTests
import HaddockTests
import PositionMappingTests
import WatchedFileTests
import CradleTests
import DependentFileTest
import NonLspCommandLine
import IfaceTests
import BootTests
import RootUriTests
import AsyncTests
import ClientSettingsTests
import ReferenceTests
import GarbageCollectionTests
import ExceptionTests

main :: IO ()
main = do
  docWithPriorityRecorder <- makeDefaultStderrRecorder (Just [PriorityColumn, DataColumn])

  let docWithFilteredPriorityRecorder@Recorder{ logger_ } =
        docWithPriorityRecorder
        & cfilter (\WithPriority{ priority } -> priority >= Debug)

  -- exists so old-style logging works. intended to be phased out
  let logger = Logger $ \p m -> logger_ (WithPriority p emptyCallStack (pretty m))

  let recorder = docWithFilteredPriorityRecorder
               & cmapWithPrio pretty

  -- We mess with env vars so run single-threaded.
  defaultMainWithRerun $ testGroup "ghcide"
    [ OpenCloseTest.tests
    , InitializeResponseTests.tests
    , CompletionTests.tests
    , CPPTests.tests
    , DiagnosticTests.tests
    , CodeLensTests.tests
    , OutlineTests.tests
    , HighlightTests.tests
    , FindDefinitionAndHoverTests.tests
    , PluginSimpleTests.tests
    , PreprocessorTests.tests
    , THTests.tests
    , SymlinkTests.tests
    , SafeTests.tests
    , UnitTests.tests recorder logger
    , HaddockTests.tests
    , PositionMappingTests.tests
    , WatchedFileTests.tests
    , CradleTests.tests
    , DependentFileTest.tests
    , NonLspCommandLine.tests
    , IfaceTests.tests
    , BootTests.tests
    , RootUriTests.tests
    , AsyncTests.tests
    , ClientSettingsTests.tests
    , ReferenceTests.tests
    , GarbageCollectionTests.tests
    , HieDbRetry.tests
    , ExceptionTests.tests recorder logger
    ]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module Main
  ( main
  ) where

import           Control.Lens               (_Just, folded, preview, view, (^.),
                                             (^..), (^?))
import           Control.Monad              (join)
import           Data.Aeson                 (Value (Object), fromJSON, object,
                                             (.=))
import           Data.Aeson.Types           (Pair, Result (Success))
import           Data.List                  (isInfixOf)
import           Data.List.Extra            (nubOrdOn)
import qualified Data.Map                   as Map
import qualified Data.Maybe                 as Maybe
import qualified Data.Text                  as T
import           Ide.Plugin.Config          (Config)
import qualified Ide.Plugin.Config          as Plugin
import qualified Ide.Plugin.Eval            as Eval
import           Ide.Plugin.Eval.Types      (EvalParams (..), Section (..),
                                             testOutput)
import           Language.LSP.Protocol.Lens (command, range, title)
import           System.FilePath            ((<.>), (</>))
import           Test.Hls
import qualified Test.Hls.FileSystem        as FS

main :: IO ()
main = defaultTestRunner tests

evalPlugin :: PluginTestDescriptor Eval.Log
evalPlugin = mkPluginTestDescriptor Eval.descriptor "eval"

tests :: TestTree
tests =
  testGroup "eval"
  [ testCase "Produces Evaluate code lenses" $
      runSessionWithServerInTmpDir def evalPlugin (mkFs $ FS.directProject "T1.hs") $ do
        doc <- openDoc "T1.hs" "haskell"
        lenses <- getCodeLenses doc
        liftIO $ map (preview $ command . _Just . title) lenses @?= [Just "Evaluate..."]
  , testCase "Produces Refresh code lenses" $
      runSessionWithServerInTmpDir def evalPlugin (mkFs $ FS.directProject "T2.hs") $ do
        doc <- openDoc "T2.hs" "haskell"
        lenses <- getCodeLenses doc
        liftIO $ map (preview $ command . _Just . title) lenses @?= [Just "Refresh..."]
  , testCase "Code lenses have ranges" $
      runSessionWithServerInTmpDir def evalPlugin (mkFs $ FS.directProject "T1.hs") $ do
        doc <- openDoc "T1.hs" "haskell"
        lenses <- getCodeLenses doc
        liftIO $ map (view range) lenses @?= [Range (Position 4 0) (Position 5 0)]
  , testCase "Multi-line expressions have a multi-line range" $ do
      runSessionWithServerInTmpDir def evalPlugin (mkFs $ FS.directProject "T3.hs") $ do
        doc <- openDoc "T3.hs" "haskell"
        lenses <- getCodeLenses doc
        liftIO $ map (view range) lenses @?= [Range (Position 3 0) (Position 5 0)]
  , testCase "Executed expressions range covers only the expression" $ do
      runSessionWithServerInTmpDir def evalPlugin (mkFs $ FS.directProject "T2.hs") $ do
        doc <- openDoc "T2.hs" "haskell"
        lenses <- getCodeLenses doc
        liftIO $ map (view range) lenses @?= [Range (Position 4 0) (Position 5 0)]

  , goldenWithEvalForCodeAction "Evaluation of expressions via code action" "T1" "hs"
  , goldenWithEvalForCodeAction "Reevaluation of expressions via code action" "T2" "hs"

  , goldenWithEval "Evaluation of expressions" "T1" "hs"
  , goldenWithEval "Reevaluation of expressions" "T2" "hs"
  , goldenWithEval "Evaluation of expressions w/ imports" "T3" "hs"
  , goldenWithEval "Evaluation of expressions w/ lets" "T4" "hs"
  , goldenWithEval "Refresh an evaluation" "T5" "hs"
  , goldenWithEval "Refresh an evaluation w/ lets" "T6" "hs"
  , goldenWithEval "Refresh a multiline evaluation" "T7" "hs"
  , goldenWithEval "Evaluate a multi-line show result" "TMultiResult" "hs" -- Do not escape from comments!
  , testCase "Semantic and Lexical errors are reported" $ do
      evalInFile "T8.hs" "-- >>> noFunctionWithThisName" "-- Variable not in scope: noFunctionWithThisName"
      evalInFile "T8.hs" "-- >>> res = \"a\" + \"bc\"" $
        if ghcVersion >= GHC96 then
          "-- No instance for `Num String' arising from a use of `+'\n-- In the expression: \"a\" + \"bc\"\n-- In an equation for `res': res = \"a\" + \"bc\""
        else
          "-- No instance for (Num String) arising from a use of `+'\n-- In the expression: \"a\" + \"bc\"\n-- In an equation for `res': res = \"a\" + \"bc\""

      evalInFile "T8.hs" "-- >>> \"" (if ghcVersion >= GHC912 then "-- lexical error at end of input" else "-- lexical error in string/character literal at end of input")
      evalInFile "T8.hs" "-- >>> 3 `div` 0" "-- divide by zero" -- The default for marking exceptions is False
  , goldenWithEval "Applies file LANGUAGE extensions" "T9" "hs"
  , goldenWithEval "Evaluate a type with :kind!" "T10" "hs"
  , goldenWithEval "Reports an error for an incorrect type with :kind!" "T11" "hs"
  , goldenWithEval "Shows a kind with :kind" "T12" "hs"
  , goldenWithEval "Reports an error for an incorrect type with :kind" "T13" "hs"
  , goldenWithEval' "Returns a fully-instantiated type for :type" "T14" "hs" (if ghcVersion >= GHC98 then "ghc98.expected" else "expected") -- See https://gitlab.haskell.org/ghc/ghc/-/issues/24069
  , goldenWithEval "Doesn't break in module containing main function" "T4139" "hs"
  , goldenWithEval "Returns defaulted type for :type +d, admitting multiple whitespaces around arguments" "T16" "hs"
  , goldenWithEval ":type reports an error when given with unknown +x option" "T17" "hs"
  , goldenWithEval "Reports an error when given with unknown command" "T18" "hs"
  , goldenWithEval "Returns defaulted type for :type +d reflecting the default declaration specified in the >>> prompt" "T19" "hs"
  -- TODO: known issue - see a note in P.R. #361
  , goldenWithEval ":type +d reflects the `default' declaration of the module" "T20" "hs"
  , testCase ":type handles a multilined result properly" $
      evalInFile "T21.hs" "-- >>> :type fun" $ T.unlines [
        "-- fun",
        "--   :: forall {k1} (k2 :: Nat) (n :: Nat) (a :: k1).",
        "--      (KnownNat k2, KnownNat n, Typeable a) =>",
        "--      Proxy k2 -> Proxy n -> Proxy a -> ()"
      ]
  , goldenWithEval ":t behaves exactly the same as :type" "T22" "hs"
  , testCase ":type does \"dovetails\" for short identifiers" $
      evalInFile "T23.hs" "-- >>> :type f" $ T.unlines [
        "-- f :: forall {k1} (k2 :: Nat) (n :: Nat) (a :: k1).",
        "--      (KnownNat k2, KnownNat n, Typeable a) =>",
        "--      Proxy k2 -> Proxy n -> Proxy a -> ()"
      ]
  , goldenWithEval ":kind! treats a multilined result properly" "T24" "hs"
  , goldenWithEval ":kind treats a multilined result properly" "T25" "hs"
  , goldenWithEvalAndFs "local imports" (FS.directProjectMulti ["T26.hs", "Util.hs"]) "T26" "hs"
  , goldenWithEval "Preserves one empty comment line after prompt" "T27" "hs"
  , goldenWithEval "Evaluate comment after multiline function definition" "T28" "hs"
  , goldenWithEval "Multi line comments" "TMulti" "hs"
  , goldenWithEval "Multi line comments, with the last test line ends without newline" "TEndingMulti" "hs"
  , goldenWithEval "Evaluate expressions in Plain comments in both single line and multi line format" "TPlainComment" "hs"
  , goldenWithEval "Evaluate expressions in Haddock comments in both single line and multi line format" "THaddock" "hs"
  , goldenWithEval "Compare results (for Haddock tests only)" "TCompare" "hs"
  , goldenWithEvalAndFs "Local Modules imports are accessible in a test" (FS.directProjectMulti ["TLocalImport.hs", "Util.hs"]) "TLocalImport" "hs"
  , goldenWithEvalAndFs "Transitive local dependency"  (FS.directProjectMulti ["TTransitive.hs", "TLocalImport.hs", "Util.hs"]) "TTransitive" "hs"
  -- , goldenWithEval "Local Modules can be imported in a test" "TLocalImportInTest" "hs"
  , goldenWithEval "Setting language option TupleSections" "TLanguageOptionsTupleSections" "hs"
  , goldenWithEval' ":set accepts ghci flags" "TFlags" "hs" (if ghcVersion >= GHC98 then "ghc98.expected" else "expected")
  , testCase ":set -fprint-explicit-foralls works" $ do
      evalInFile "T8.hs" "-- >>> :t id" "-- id :: a -> a"
      evalInFile "T8.hs" "-- >>> :set -fprint-explicit-foralls\n-- >>> :t id" "-- id :: forall a. a -> a"
  , goldenWithEval "The default language extensions for the eval plugin are the same as those for ghci" "TSameDefaultLanguageExtensionsAsGhci" "hs"
  , goldenWithEval "IO expressions are supported, stdout/stderr output is ignored" "TIO" "hs"
  , goldenWithEvalAndFs "Property checking" cabalProjectFS "TProperty" "hs"
  , knownBrokenInWindowsBeforeGHC912 "The output has path separators in it, which on Windows look different. Just skip it there" $
      goldenWithEvalAndFs' "Property checking with exception" cabalProjectFS "TPropertyError" "hs" $
        case ghcVersion of
          GHC912 -> "ghc912.expected"
          GHC910 -> "ghc910.expected"
          GHC98  -> "ghc98.expected"
          GHC96  -> "ghc96.expected"
  , goldenWithEval "Prelude has no special treatment, it is imported as stated in the module" "TPrelude" "hs"
  , goldenWithEval "Don't panic on {-# UNPACK #-} pragma" "TUNPACK" "hs"
  , goldenWithEval "Can handle eval inside nested comment properly" "TNested" "hs"
  , goldenWithEval "Test on last line insert results correctly" "TLastLine" "hs"
  , testGroup "with preprocessors"
    [ knownBrokenInEnv [HostOS Windows]
        "CPP eval on Windows and/or GHC <= 8.6 fails for some reasons" $
        goldenWithEval "CPP support" "TCPP" "hs"
    , goldenWithEval "Literate Haskell Bird Style" "TLHS" "lhs"
    ]
  , goldenWithEval "Works with NoImplicitPrelude" "TNoImplicitPrelude" "hs"
  , goldenWithEval "Variable 'it' works" "TIt" "hs"
  , testGroup "configuration"
    [ goldenWithEval' "Give 'WAS' by default" "TDiff" "hs" "expected.default"
    , goldenWithEvalConfig' "Give the result only if diff is off" "TDiff" "hs" "expected.no-diff" diffOffConfig
    , goldenWithEvalConfig' "Evaluates to exception (not marked)" "TException" "hs" "expected.nomark" (exceptionConfig False)
    , goldenWithEvalConfig' "Evaluates to exception (with mark)" "TException" "hs" "expected.marked" (exceptionConfig True)
    ]
  , testGroup ":info command"
    [ testCase ":info reports type, constructors and instances" $ do
        [output] <- map (unlines . codeLensTestOutput) <$> evalLenses "TInfo.hs"
        "data Foo = Foo1 | Foo2" `isInfixOf` output @? "Output does not include Foo data declaration"
        "Eq Foo" `isInfixOf` output                 @? "Output does not include instance Eq Foo"
        "Ord Foo" `isInfixOf` output                @? "Output does not include instance Ord Foo"
        not ("Baz Foo" `isInfixOf` output)          @? "Output includes instance Baz Foo"
    , testCase ":info reports type, constructors and instances for multiple types" $ do
        [output] <- map (unlines . codeLensTestOutput) <$> evalLenses "TInfoMany.hs"
        "data Foo = Foo1 | Foo2" `isInfixOf` output        @? "Output does not include Foo data declaration"
        "Eq Foo" `isInfixOf` output                        @? "Output does not include instance Eq Foo"
        "Ord Foo" `isInfixOf` output                       @? "Output does not include instance Ord Foo"
        not ("Baz Foo" `isInfixOf` output)                 @? "Output includes instance Baz Foo"
        "data Bar = Bar1 | Bar2 | Bar3" `isInfixOf` output @? "Output does not include Bar data declaration"
        "Eq Bar" `isInfixOf` output                        @? "Output does not include instance Eq Bar"
        "Ord Bar" `isInfixOf` output                       @? "Output does not include instance Ord Bar"
        not ("Baz Bar" `isInfixOf` output)                 @? "Output includes instance Baz Bar"
    , testCase ":info! reports type, constructors and unfiltered instances" $ do
        [output] <- map (unlines . codeLensTestOutput) <$> evalLenses "TInfoBang.hs"
        "data Foo = Foo1 | Foo2" `isInfixOf` output @? "Output does not include Foo data declaration"
        "Eq Foo" `isInfixOf` output                 @? "Output does not include instance Eq Foo"
        "Ord Foo" `isInfixOf` output                @? "Output does not include instance Ord Foo"
        "Baz Foo" `isInfixOf` output                @? "Output does not include instance Baz Foo"
    , testCase ":info! reports type, constructors and unfiltered instances for multiple types" $ do
        [output] <- map (unlines . codeLensTestOutput) <$> evalLenses "TInfoBangMany.hs"
        "data Foo = Foo1 | Foo2" `isInfixOf` output        @? "Output does not include Foo data declaration"
        "Eq Foo" `isInfixOf` output                        @? "Output does not include instance Eq Foo"
        "Ord Foo" `isInfixOf` output                       @? "Output does not include instance Ord Foo"
        "Baz Foo" `isInfixOf` output                       @? "Output does not include instance Baz Foo"
        "data Bar = Bar1 | Bar2 | Bar3" `isInfixOf` output @? "Output does not include Bar data declaration"
        "Eq Bar" `isInfixOf` output                        @? "Output does not include instance Eq Bar"
        "Ord Bar" `isInfixOf` output                       @? "Output does not include instance Ord Bar"
        "Baz Bar" `isInfixOf` output                       @? "Output does not include instance Baz Bar"
    , testCase ":i behaves exactly the same as :info" $ do
        [output] <- map (unlines . codeLensTestOutput) <$> evalLenses "TI_Info.hs"
        "data Foo = Foo1 | Foo2" `isInfixOf` output @? "Output does not include Foo data declaration"
        "Eq Foo" `isInfixOf` output                 @? "Output does not include instance Eq Foo"
        "Ord Foo" `isInfixOf` output                @? "Output does not include instance Ord Foo"
        not ("Baz Foo" `isInfixOf` output)          @? "Output includes instance Baz Foo"
    ]
  , testCase "Interfaces are reused after Eval" $ do
      runSessionWithServerInTmpDir def evalPlugin (mkFs $ FS.directProjectMulti ["TLocalImport.hs", "Util.hs"]) $ do
        doc <- openDoc "TLocalImport.hs" "haskell"
        _ <- waitForTypecheck doc
        lenses <- getCodeLenses doc
        cmd <- liftIO $ case lenses^..folded.command._Just of
          [cmd] -> (cmd^.title @?= "Evaluate...") >> pure cmd
          cmds -> assertFailure $ "Expected a single command, got " <> show (length cmds)

        executeCmd cmd

        -- trigger a rebuild and check that dependency interfaces are not rebuilt
        changeDoc doc []
        _ <- waitForTypecheck doc
        Right keys <- getLastBuildKeys
        let ifaceKeys = filter ("GetModIface" `T.isPrefixOf`) keys
        liftIO $ ifaceKeys @?= []
  ]
  where
    knownBrokenInWindowsBeforeGHC912 msg =
        foldl (.) id
           [ knownBrokenInSpecificEnv [GhcVer ghcVer, HostOS Windows] msg
           | ghcVer <- [GHC96 .. GHC910]
           ]

goldenWithEval :: TestName -> FilePath -> FilePath -> TestTree
goldenWithEval title path ext =
  goldenWithHaskellDocInTmpDir def evalPlugin title (mkFs $ FS.directProject (path <.> ext)) path "expected" ext executeLensesBackwards

goldenWithEvalForCodeAction :: TestName -> FilePath -> FilePath -> TestTree
goldenWithEvalForCodeAction title path ext =
  goldenWithHaskellDocInTmpDir def evalPlugin title (mkFs $ FS.directProject (path <.> ext)) path "expected" ext executeCodeActionsBackwards

goldenWithEvalAndFs :: TestName -> [FS.FileTree] -> FilePath -> FilePath -> TestTree
goldenWithEvalAndFs title tree path ext =
  goldenWithHaskellDocInTmpDir def evalPlugin title (mkFs tree) path  "expected" ext executeLensesBackwards

-- | Similar function as 'goldenWithEval' with an alternate reference file
-- naming. Useful when reference file may change because of GHC version.
goldenWithEval' :: TestName -> FilePath -> FilePath -> FilePath -> TestTree
goldenWithEval' title path ext expected =
  goldenWithHaskellDocInTmpDir def evalPlugin title (mkFs $ FS.directProject (path <.> ext)) path expected ext executeLensesBackwards

goldenWithEvalAndFs' :: TestName -> [FS.FileTree] ->  FilePath -> FilePath -> FilePath -> TestTree
goldenWithEvalAndFs' title tree path ext expected =
  goldenWithHaskellDocInTmpDir def evalPlugin title (mkFs tree) path expected ext executeLensesBackwards


-- | Execute lenses backwards, to avoid affecting their position in the source file
executeLensesBackwards :: TextDocumentIdentifier -> Session ()
executeLensesBackwards doc = do
  codeLenses <- getCodeLenses doc
  -- liftIO $ print codeLenses
  executeCmdsBackwards [c | CodeLens{_command = Just c} <- codeLenses]

executeCodeActionsBackwards :: TextDocumentIdentifier -> Session ()
executeCodeActionsBackwards doc = do
  codeLenses <- getCodeLenses doc
  let ranges = [_range | CodeLens{_range} <- codeLenses]
  -- getAllCodeActions cannot get our code actions because they have no diagnostics
  codeActions <- join <$> traverse (getCodeActions doc) ranges
  let cmds = Maybe.mapMaybe (^? _L) codeActions
  executeCmdsBackwards cmds

-- Execute commands backwards, nubbing elements to avoid
-- evaluating the same section with multiple tests
-- more than twice
executeCmdsBackwards :: [Command] -> Session ()
executeCmdsBackwards = mapM_ executeCmd . nubOrdOn actSectionId . reverse

actSectionId :: Command -> Int
actSectionId Command{_arguments = Just [fromJSON -> Success EvalParams{..}]} = evalId
actSectionId _ = error "Invalid CodeLens"

-- Execute command and wait for result
executeCmd :: Command -> Session ()
executeCmd cmd = do
  executeCommand cmd
  _ <- skipManyTill anyMessage (message SMethod_WorkspaceApplyEdit)
  -- liftIO $ print _resp
  pure ()

evalLenses :: FilePath -> IO [CodeLens]
evalLenses path = runSessionWithServerInTmpDir def evalPlugin (mkFs cabalProjectFS) $ do
  doc <- openDoc path "haskell"
  executeLensesBackwards doc
  getCodeLenses doc

codeLensTestOutput :: CodeLens -> [String]
codeLensTestOutput codeLens = do
  CodeLens { _command = Just command } <- [codeLens]
  Command { _arguments = Just args } <- [command]
  Success EvalParams { sections = sections } <- fromJSON @EvalParams <$> args
  Section { sectionTests = sectionTests } <- sections
  testOutput =<< sectionTests

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-eval-plugin" </> "test" </> "testdata"

changeConfig :: [Pair] -> Config
changeConfig conf =
  def
    { Plugin.plugins = Map.fromList [("eval",
        def { Plugin.plcGlobalOn = True, Plugin.plcConfig = unObject $ object conf }
    )] }
  where
    unObject (Object obj) = obj
    unObject _            = undefined

diffOffConfig :: Config
diffOffConfig = changeConfig ["diff" .= False]

exceptionConfig :: Bool -> Config
exceptionConfig exCfg = changeConfig ["exception" .= exCfg]

goldenWithEvalConfig' :: TestName -> FilePath -> FilePath -> FilePath -> Config -> TestTree
goldenWithEvalConfig' title path ext expected cfg =
  goldenWithHaskellDocInTmpDir cfg evalPlugin title (mkFs $ FS.directProject $ path <.> ext) path expected ext $ \doc -> do
    executeLensesBackwards doc

evalInFile :: HasCallStack => FilePath -> T.Text -> T.Text -> IO ()
evalInFile fp e expected = runSessionWithServerInTmpDir def evalPlugin (mkFs $ FS.directProject fp) $ do
  doc <- openDoc fp "haskell"
  origin <- documentContents doc
  let withEval = origin <> e
  changeDoc doc [TextDocumentContentChangeEvent . InR . TextDocumentContentChangeWholeDocument $ withEval]
  executeLensesBackwards doc
  result <- fmap T.strip . T.stripPrefix withEval <$> documentContents doc
  liftIO $ result @?= Just (T.strip expected)

-- ----------------------------------------------------------------------------
-- File system definitions
-- Used for declaring a test file tree
-- ----------------------------------------------------------------------------

mkFs :: [FS.FileTree] -> FS.VirtualFileTree
mkFs = FS.mkVirtualFileTree testDataDir

cabalProjectFS :: [FS.FileTree]
cabalProjectFS = FS.simpleCabalProject'
  [ FS.copy "test.cabal"
  , FS.file "cabal.project"
      (FS.text "packages: ./info-util .\n"
      )
  , FS.copy "TProperty.hs"
  , FS.copy "TPropertyError.hs"
  , FS.copy "TI_Info.hs"
  , FS.copy "TInfo.hs"
  , FS.copy "TInfoBang.hs"
  , FS.copy "TInfoBangMany.hs"
  , FS.copy "TInfoMany.hs"
  , FS.directory "info-util"
    [ FS.copy "info-util/info-util.cabal"
    , FS.copy "info-util/InfoUtil.hs"
    ]
  ]

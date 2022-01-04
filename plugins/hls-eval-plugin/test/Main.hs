{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module Main
  ( main
  ) where

import           Control.Lens            (_Just, folded, preview, toListOf,
                                          view, (^..))
import           Data.Aeson              (fromJSON)
import           Data.Aeson.Types        (Result (Success))
import           Data.List               (isInfixOf)
import           Data.List.Extra         (nubOrdOn)
import qualified Data.Text               as T
import qualified Ide.Plugin.Eval         as Eval
import           Ide.Plugin.Eval.Types   (EvalParams (..), Section (..),
                                          testOutput)
import           Language.LSP.Types.Lens (arguments, command, range, title)
import           System.FilePath         ((</>))
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

evalPlugin :: PluginDescriptor IdeState
evalPlugin = Eval.descriptor "eval"

tests :: TestTree
tests =
  testGroup "eval"
  [ testCase "Produces Evaluate code lenses" $
      runSessionWithServer evalPlugin testDataDir $ do
        doc <- openDoc "T1.hs" "haskell"
        lenses <- getCodeLenses doc
        liftIO $ map (preview $ command . _Just . title) lenses @?= [Just "Evaluate..."]
  , testCase "Produces Refresh code lenses" $
      runSessionWithServer evalPlugin testDataDir $ do
        doc <- openDoc "T2.hs" "haskell"
        lenses <- getCodeLenses doc
        liftIO $ map (preview $ command . _Just . title) lenses @?= [Just "Refresh..."]
  , testCase "Code lenses have ranges" $
      runSessionWithServer evalPlugin testDataDir $ do
        doc <- openDoc "T1.hs" "haskell"
        lenses <- getCodeLenses doc
        liftIO $ map (view range) lenses @?= [Range (Position 4 0) (Position 5 0)]
  , testCase "Multi-line expressions have a multi-line range" $ do
      runSessionWithServer evalPlugin testDataDir $ do
        doc <- openDoc "T3.hs" "haskell"
        lenses <- getCodeLenses doc
        liftIO $ map (view range) lenses @?= [Range (Position 3 0) (Position 5 0)]
  , testCase "Executed expressions range covers only the expression" $ do
      runSessionWithServer evalPlugin testDataDir $ do
        doc <- openDoc "T2.hs" "haskell"
        lenses <- getCodeLenses doc
        liftIO $ map (view range) lenses @?= [Range (Position 4 0) (Position 5 0)]

  , goldenWithEval "Evaluation of expressions" "T1" "hs"
  , goldenWithEval "Reevaluation of expressions" "T2" "hs"
  , goldenWithEval "Evaluation of expressions w/ imports" "T3" "hs"
  , goldenWithEval "Evaluation of expressions w/ lets" "T4" "hs"
  , goldenWithEval "Refresh an evaluation" "T5" "hs"
  , goldenWithEval "Refresh an evaluation w/ lets" "T6" "hs"
  , goldenWithEval "Refresh a multiline evaluation" "T7" "hs"
  , testCase "Semantic and Lexical errors are reported" $ do
      evalInFile "T8.hs" "-- >>> noFunctionWithThisName" "-- Variable not in scope: noFunctionWithThisName"
      evalInFile "T8.hs" "-- >>> \"a\" + \"bc\"" $
        if ghcVersion == GHC90
          then "-- No instance for (Num String) arising from a use of ‘+’"
          else "-- No instance for (Num [Char]) arising from a use of ‘+’"
      evalInFile "T8.hs" "-- >>> \"" "-- lexical error in string/character literal at end of input"
      evalInFile "T8.hs" "-- >>> 3 `div` 0" "-- divide by zero"
  , goldenWithEval "Applies file LANGUAGE extensions" "T9" "hs"
  , goldenWithEval "Evaluate a type with :kind!" "T10" "hs"
  , goldenWithEval "Reports an error for an incorrect type with :kind!" "T11" "hs"
  , goldenWithEval "Shows a kind with :kind" "T12" "hs"
  , goldenWithEval "Reports an error for an incorrect type with :kind" "T13" "hs"
  , goldenWithEval "Returns a fully-instantiated type for :type" "T14" "hs"
  , goldenWithEval "Returns an uninstantiated type for :type +v, admitting multiple whitespaces around arguments" "T15" "hs"
  , goldenWithEval "Returns defaulted type for :type +d, admitting multiple whitespaces around arguments" "T16" "hs"
  , goldenWithEval ":type reports an error when given with unknown +x option" "T17" "hs"
  , goldenWithEval "Reports an error when given with unknown command" "T18" "hs"
  , goldenWithEval "Returns defaulted type for :type +d reflecting the default declaration specified in the >>> prompt" "T19" "hs"
  , expectFailBecause "known issue - see a note in P.R. #361" $
      goldenWithEval ":type +d reflects the `default' declaration of the module" "T20" "hs"
  , testCase ":type handles a multilined result properly" $
      evalInFile "T21.hs" "-- >>> :type fun" $ T.unlines [
        "-- fun",
        if ghcVersion == GHC90
          then "--   :: forall {k1} {k2 :: Nat} {n :: Nat} {a :: k1}."
          else "--   :: forall k1 (k2 :: Nat) (n :: Nat) (a :: k1).",
        "--      (KnownNat k2, KnownNat n, Typeable a) =>",
        "--      Proxy k2 -> Proxy n -> Proxy a -> ()"
      ]
  , goldenWithEval ":t behaves exactly the same as :type" "T22" "hs"
  , testCase ":type does \"dovetails\" for short identifiers" $
      evalInFile "T23.hs" "-- >>> :type f" $ T.unlines [
        if ghcVersion == GHC90
          then "-- f :: forall {k1} {k2 :: Nat} {n :: Nat} {a :: k1}."
          else "-- f :: forall k1 (k2 :: Nat) (n :: Nat) (a :: k1).",
        "--      (KnownNat k2, KnownNat n, Typeable a) =>",
        "--      Proxy k2 -> Proxy n -> Proxy a -> ()"
      ]
  , goldenWithEval ":kind! treats a multilined result properly" "T24" "hs"
  , goldenWithEval ":kind treats a multilined result properly" "T25" "hs"
  , goldenWithEval "local imports" "T26" "hs"
  , goldenWithEval "Preserves one empty comment line after prompt" "T27" "hs"
  , goldenWithEval "Multi line comments" "TMulti" "hs"
  , goldenWithEval "Multi line comments, with the last test line ends without newline" "TEndingMulti" "hs"
  , goldenWithEval "Evaluate expressions in Plain comments in both single line and multi line format" "TPlainComment" "hs"
  , goldenWithEval "Evaluate expressions in Haddock comments in both single line and multi line format" "THaddock" "hs"
  , goldenWithEval "Compare results (for Haddock tests only)" "TCompare" "hs"
  , goldenWithEval "Local Modules imports are accessible in a test" "TLocalImport" "hs"
  , goldenWithEval "Transitive local dependency" "TTransitive" "hs"
  -- , goldenWithEval "Local Modules can be imported in a test" "TLocalImportInTest" "hs"
  , goldenWithEval "Setting language option TupleSections" "TLanguageOptionsTupleSections" "hs"
  , goldenWithEval ":set accepts ghci flags" "TFlags" "hs"
  , testCase ":set -fprint-explicit-foralls works" $ do
      evalInFile "T8.hs" "-- >>> :t id" "-- id :: a -> a"
      evalInFile "T8.hs" "-- >>> :set -fprint-explicit-foralls\n-- >>> :t id"
        "-- id :: forall {a}. a -> a"
  , goldenWithEval "The default language extensions for the eval plugin are the same as those for ghci" "TSameDefaultLanguageExtensionsAsGhci" "hs"
  , goldenWithEval "IO expressions are supported, stdout/stderr output is ignored" "TIO" "hs"
  , goldenWithEval "Property checking" "TProperty" "hs"
  , goldenWithEval "Prelude has no special treatment, it is imported as stated in the module" "TPrelude" "hs"
  , goldenWithEval "Don't panic on {-# UNPACK #-} pragma" "TUNPACK" "hs"
  , goldenWithEval "Can handle eval inside nested comment properly" "TNested" "hs"
  , goldenWithEval "Test on last line insert results correctly" "TLastLine" "hs"
  , testGroup "with preprocessors"
    [ knownBrokenInEnv [HostOS Windows, GhcVer GHC86]
        "CPP eval on Windows and/or GHC <= 8.6 fails for some reasons" $
          goldenWithEval "CPP support" "TCPP" "hs"
    , knownBrokenForGhcVersions [GHC86]
        "Preprocessor known to fail on GHC <= 8.6" $
          goldenWithEval "Literate Haskell Bird Style" "TLHS" "lhs"
    -- , goldenWithEval "Literate Haskell LaTeX Style" "TLHSLateX" "lhs"
    ]
  , goldenWithEval "Works with NoImplicitPrelude" "TNoImplicitPrelude" "hs"
  , goldenWithEval "Variable 'it' works" "TIt" "hs"

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
      runSessionWithServer evalPlugin testDataDir $ do
        doc <- openDoc "TLocalImport.hs" "haskell"
        waitForTypecheck doc
        lenses <- getCodeLenses doc
        let ~cmds@[cmd] = lenses^..folded.command._Just
        liftIO $ cmds^..folded.title @?= ["Evaluate..."]

        executeCmd cmd

        -- trigger a rebuild and check that dependency interfaces are not rebuilt
        changeDoc doc []
        waitForTypecheck doc
        Right keys <- getLastBuildKeys
        let ifaceKeys = filter ("GetModIface" `T.isPrefixOf`) keys
        liftIO $ ifaceKeys @?= []
  ]

goldenWithEval :: TestName -> FilePath -> FilePath -> TestTree
goldenWithEval title path ext =
  goldenWithHaskellDoc evalPlugin title testDataDir path "expected" ext executeLensesBackwards

-- | Execute lenses backwards, to avoid affecting their position in the source file
executeLensesBackwards :: TextDocumentIdentifier -> Session ()
executeLensesBackwards doc = do
  codeLenses <- reverse <$> getCodeLenses doc
  -- liftIO $ print codeLenses

  -- Execute sequentially, nubbing elements to avoid
  -- evaluating the same section with multiple tests
  -- more than twice
  mapM_ executeCmd $
    nubOrdOn actSectionId [c | CodeLens{_command = Just c} <- codeLenses]

actSectionId :: Command -> Int
actSectionId Command{_arguments = Just (List [fromJSON -> Success EvalParams{..}])} = evalId
actSectionId _ = error "Invalid CodeLens"

-- Execute command and wait for result
executeCmd :: Command -> Session ()
executeCmd cmd = do
  executeCommand cmd
  _ <- skipManyTill anyMessage (message SWorkspaceApplyEdit)
  -- liftIO $ print _resp
  pure ()

evalLenses :: FilePath -> IO [CodeLens]
evalLenses path = runSessionWithServer evalPlugin testDataDir $ do
  doc <- openDoc path "haskell"
  executeLensesBackwards doc
  getCodeLenses doc

codeLensTestOutput :: CodeLens -> [String]
codeLensTestOutput codeLens = do
  CodeLens { _command = Just command } <- [codeLens]
  Command { _arguments = Just (List args) } <- [command]
  Success EvalParams { sections = sections } <- fromJSON @EvalParams <$> args
  Section { sectionTests = sectionTests } <- sections
  testOutput =<< sectionTests

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

evalInFile :: FilePath -> T.Text -> T.Text -> IO ()
evalInFile fp e expected = runSessionWithServer evalPlugin testDataDir $ do
  doc <- openDoc fp "haskell"
  origin <- documentContents doc
  let withEval = origin <> e
  changeDoc doc [TextDocumentContentChangeEvent Nothing Nothing withEval]
  executeLensesBackwards doc
  result <- fmap T.strip . T.stripPrefix withEval <$> documentContents doc
  liftIO $ result @?= Just (T.strip expected)

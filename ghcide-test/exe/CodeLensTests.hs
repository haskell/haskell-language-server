{-# LANGUAGE GADTs #-}

module CodeLensTests (tests) where

import           Config
import           Control.Applicative.Combinators
import           Control.Lens                    ((^.))
import           Control.Monad                   (void)
import           Control.Monad.IO.Class          (liftIO)
import qualified Data.Aeson                      as A
import           Data.Maybe
import qualified Data.Text                       as T
import           Development.IDE.GHC.Compat      (GhcVersion (..), ghcVersion)
import qualified Language.LSP.Protocol.Lens      as L
import           Language.LSP.Protocol.Types     hiding
                                                 (SemanticTokenAbsolute (..),
                                                  SemanticTokenRelative (..),
                                                  SemanticTokensEdit (..),
                                                  mkRange)
import           Language.LSP.Test
import           Test.Hls                        (mkRange, waitForProgressDone)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "code lenses"
  [ addSigLensesTests
  ]

data TestSpec =
  TestSpec
    { mName :: Maybe TestName -- ^ Optional Test Name
    , input :: T.Text -- ^ Input
    , expected :: Maybe T.Text -- ^ Expected Type Sig
    }

mkT :: T.Text -> T.Text -> TestSpec
mkT i e = TestSpec Nothing i (Just e)
mkT' :: TestName -> T.Text -> T.Text -> TestSpec
mkT' name i e  = TestSpec (Just name) i (Just e)

noExpected :: TestSpec -> TestSpec
noExpected t = t { expected = Nothing }

mkTestName :: TestSpec -> String
mkTestName t = case mName t of
  Nothing -> T.unpack $ T.replace "\n" "\\n" (input t)
  Just name -> name

addSigLensesTests :: TestTree
addSigLensesTests =
  let pragmas = "{-# OPTIONS_GHC -Wmissing-signatures -Wmissing-pattern-synonym-signatures #-}"
      moduleH exported =
        T.unlines
          [ "{-# LANGUAGE PatternSynonyms,TypeApplications,DataKinds,RankNTypes,ScopedTypeVariables,TypeOperators,GADTs,BangPatterns #-}"
          , "module Sigs(" <> exported <> ") where"
          , "import qualified Data.Complex as C"
          , "import Data.Data (Proxy (..), type (:~:) (..), mkCharType)"
          , "data T1 a where"
          , "  MkT1 :: (Show b) => a -> b -> T1 a"
          ]
      before enableGHCWarnings exported spec others =
        T.unlines $ [pragmas | enableGHCWarnings] <> [moduleH exported, input spec] <> others
      after' enableGHCWarnings exported spec others =
        T.unlines $ [pragmas | enableGHCWarnings] <> [moduleH exported] <> maybe [] pure (expected spec) <> [input spec] <> others
      createConfig mode = A.object ["plugin" A..= A.object ["ghcide-type-lenses" A..= A.object ["config" A..= A.object ["mode" A..= A.String mode]]]]
      sigSession testName enableGHCWarnings waitForDiags mode exported spec others = testWithDummyPluginEmpty testName $ do
        let originalCode = before enableGHCWarnings exported spec others
        let expectedCode = after' enableGHCWarnings exported spec others
        setConfigSection "haskell" (createConfig mode)
        doc <- createDoc "Sigs.hs" "haskell" originalCode
        -- Because the diagnostics mode is really relying only on diagnostics now
        -- to generate the code lens we need to make sure we wait till the file
        -- is parsed before asking for codelenses, otherwise we will get nothing.
        if waitForDiags
          then void waitForDiagnostics
          else waitForProgressDone
        codeLenses <- getAndResolveCodeLenses doc
        if isJust $ expected spec
          then do
            liftIO $ length codeLenses == 1 @? "Expected 1 code lens, but got: " <> show codeLenses
            executeCommand $ fromJust $ head codeLenses ^. L.command
            modifiedCode <- skipManyTill anyMessage (getDocumentEdit doc)
            liftIO $ expectedCode @=? modifiedCode
          else liftIO $ null codeLenses @? "Expected no code lens, but got: " <> show codeLenses
      cases =
        [ mkT "abc = True" "abc :: Bool"
        , mkT "foo a b = a + b" "foo :: Num a => a -> a -> a"
        , mkT "bar a b = show $ a + b" "bar :: (Show a, Num a) => a -> a -> String"
        , mkT "(!!!) a b = a > b" "(!!!) :: Ord a => a -> a -> Bool"
        , mkT "a >>>> b = a + b" "(>>>>) :: Num a => a -> a -> a"
        , mkT "a `haha` b = a b" "haha :: (t1 -> t2) -> t1 -> t2"
        , mkT "pattern Some a = Just a" "pattern Some :: a -> Maybe a"
        , mkT "pattern Some a <- Just a" "pattern Some :: a -> Maybe a"
        , mkT "pattern Some a <- Just a\n  where Some a = Just a" "pattern Some :: a -> Maybe a"
        , mkT "pattern Some a <- Just !a\n  where Some !a = Just a" "pattern Some :: a -> Maybe a"
        , mkT "pattern Point{x, y} = (x, y)" "pattern Point :: a -> b -> (a, b)"
        , mkT "pattern Point{x, y} <- (x, y)" "pattern Point :: a -> b -> (a, b)"
        , mkT "pattern Point{x, y} <- (x, y)\n  where Point x y = (x, y)" "pattern Point :: a -> b -> (a, b)"
        , mkT "pattern MkT1' b = MkT1 42 b" "pattern MkT1' :: (Eq a, Num a) => Show b => b -> T1 a"
        , mkT "pattern MkT1' b <- MkT1 42 b" "pattern MkT1' :: (Eq a, Num a) => Show b => b -> T1 a"
        , mkT "pattern MkT1' b <- MkT1 42 b\n  where MkT1' b = MkT1 42 b" "pattern MkT1' :: (Eq a, Num a) => Show b => b -> T1 a"
        , mkT "qualifiedSigTest= C.realPart" "qualifiedSigTest :: C.Complex a -> a"
        , mkT "head = 233" "head :: Integer"
        , mkT "rank2Test (k :: forall a . a -> a) = (k 233 :: Int, k \"QAQ\")" "rank2Test :: (forall a. a -> a) -> (Int, String)"
        , mkT "symbolKindTest = Proxy @\"qwq\"" "symbolKindTest :: Proxy \"qwq\""
        , mkT "promotedKindTest = Proxy @Nothing" (if ghcVersion >= GHC96 then "promotedKindTest :: Proxy Nothing" else "promotedKindTest :: Proxy 'Nothing")
        , mkT "typeOperatorTest = Refl" "typeOperatorTest :: forall {k} {a :: k}. a :~: a"
        , mkT "notInScopeTest = mkCharType"
          (if ghcVersion < GHC910
              then "notInScopeTest :: String -> Data.Data.DataType"
              else "notInScopeTest :: String -> GHC.Internal.Data.Data.DataType"
          )

        , mkT' "aVeryLongSignature"
          "aVeryLongSignature a b c d e f g h i j k l m n = a && b && c && d && e && f && g && h && i && j && k && l && m && n"
          "aVeryLongSignature :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool"
        ]
   in testGroup
        "add signature"
        [ testGroup "signatures are correct" [sigSession (mkTestName spec) False False "always" "" spec [] | spec <-  cases]
        , sigSession "exported mode works" False False "exported" "xyz" (mkT "xyz = True" "xyz :: Bool") (input <$> take 3 cases)
        , testGroup
            "diagnostics mode works"
            [ sigSession "with GHC warnings" True True "diagnostics" "" (head cases) []
            , sigSession "without GHC warnings" False False "diagnostics" "" (noExpected $ head cases) []
            ]
        , testWithDummyPluginEmpty "keep stale lens" $ do
            let content = T.unlines
                    [ "module Stale where"
                    , "f = _"
                    ]
            doc <- createDoc "Stale.hs" "haskell" content
            oldLens <- getCodeLenses doc
            liftIO $ length oldLens @?= 1
            let edit = TextEdit (mkRange 0 4 0 5) "" -- Remove the `_`
            _ <- applyEdit doc edit
            newLens <- getCodeLenses doc
            liftIO $ newLens @?= oldLens
        ]

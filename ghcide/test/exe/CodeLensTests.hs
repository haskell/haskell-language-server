{-# LANGUAGE GADTs #-}

module CodeLensTests (tests) where

import           Control.Applicative.Combinators
import           Control.Monad.IO.Class          (liftIO)
import qualified Data.Aeson                      as A
import           Data.Maybe
import qualified Data.Text                       as T
import           Data.Traversable                (for)
import           Development.IDE.GHC.Compat      (GhcVersion (..), ghcVersion)
import qualified Language.LSP.Protocol.Lens      as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types     hiding
                                                 (SemanticTokenAbsolute (..),
                                                  SemanticTokenRelative (..),
                                                  SemanticTokensEdit (..),
                                                  mkRange)
import           Language.LSP.Test
-- import Test.QuickCheck.Instances ()
import           Control.Exception               (throw)
import           Control.Lens                    ((^.))
import           Control.Monad                   (void)
import           Data.Tuple.Extra
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUtils

tests :: TestTree
tests = testGroup "code lenses"
  [ addSigLensesTests
  ]


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
      before enableGHCWarnings exported (def, _) others =
        T.unlines $ [pragmas | enableGHCWarnings] <> [moduleH exported, def] <> others
      after' enableGHCWarnings exported (def, sig) others =
        T.unlines $ [pragmas | enableGHCWarnings] <> [moduleH exported] <> maybe [] pure sig <> [def] <> others
      createConfig mode = A.object ["haskell" A..= A.object ["plugin" A..= A.object ["ghcide-type-lenses" A..= A.object ["config" A..= A.object ["mode" A..= A.String mode]]]]]
      sigSession testName enableGHCWarnings waitForDiags mode exported def others = testSession testName $ do
        let originalCode = before enableGHCWarnings exported def others
        let expectedCode = after' enableGHCWarnings exported def others
        sendNotification SMethod_WorkspaceDidChangeConfiguration $ DidChangeConfigurationParams $ createConfig mode
        doc <- createDoc "Sigs.hs" "haskell" originalCode
        -- Because the diagnostics mode is really relying only on diagnostics now
        -- to generate the code lens we need to make sure we wait till the file
        -- is parsed before asking for codelenses, otherwise we will get nothing.
        if waitForDiags
          then void waitForDiagnostics
          else waitForProgressDone
        codeLenses <- getAndResolveCodeLenses doc
        if not $ null $ snd def
          then do
            liftIO $ length codeLenses == 1 @? "Expected 1 code lens, but got: " <> show codeLenses
            executeCommand $ fromJust $ head codeLenses ^. L.command
            modifiedCode <- skipManyTill anyMessage (getDocumentEdit doc)
            liftIO $ expectedCode @=? modifiedCode
          else liftIO $ null codeLenses @? "Expected no code lens, but got: " <> show codeLenses
      cases =
        [ ("abc = True", "abc :: Bool")
        , ("foo a b = a + b", "foo :: Num a => a -> a -> a")
        , ("bar a b = show $ a + b", "bar :: (Show a, Num a) => a -> a -> String")
        , ("(!!!) a b = a > b", "(!!!) :: Ord a => a -> a -> Bool")
        , ("a >>>> b = a + b", "(>>>>) :: Num a => a -> a -> a")
        , ("a `haha` b = a b", "haha :: (t1 -> t2) -> t1 -> t2")
        , ("pattern Some a = Just a", "pattern Some :: a -> Maybe a")
        , ("pattern Some a <- Just a", "pattern Some :: a -> Maybe a")
        , ("pattern Some a <- Just a\n  where Some a = Just a", "pattern Some :: a -> Maybe a")
        , ("pattern Some a <- Just !a\n  where Some !a = Just a", "pattern Some :: a -> Maybe a")
        , ("pattern Point{x, y} = (x, y)", "pattern Point :: a -> b -> (a, b)")
        , ("pattern Point{x, y} <- (x, y)", "pattern Point :: a -> b -> (a, b)")
        , ("pattern Point{x, y} <- (x, y)\n  where Point x y = (x, y)", "pattern Point :: a -> b -> (a, b)")
        , ("pattern MkT1' b = MkT1 42 b", "pattern MkT1' :: (Eq a, Num a) => Show b => b -> T1 a")
        , ("pattern MkT1' b <- MkT1 42 b", "pattern MkT1' :: (Eq a, Num a) => Show b => b -> T1 a")
        , ("pattern MkT1' b <- MkT1 42 b\n  where MkT1' b = MkT1 42 b", "pattern MkT1' :: (Eq a, Num a) => Show b => b -> T1 a")
        , ("qualifiedSigTest= C.realPart", "qualifiedSigTest :: C.Complex a -> a")
        , ("head = 233", "head :: Integer")
        , ("rank2Test (k :: forall a . a -> a) = (k 233 :: Int, k \"QAQ\")", "rank2Test :: (forall a. a -> a) -> (Int, " <> listOfChar <> ")")
        , ("symbolKindTest = Proxy @\"qwq\"", "symbolKindTest :: Proxy \"qwq\"")
        , ("promotedKindTest = Proxy @Nothing", if ghcVersion >= GHC96 then "promotedKindTest :: Proxy Nothing" else "promotedKindTest :: Proxy 'Nothing")
        , ("typeOperatorTest = Refl", if ghcVersion >= GHC92 then "typeOperatorTest :: forall {k} {a :: k}. a :~: a" else "typeOperatorTest :: a :~: a")
        , ("notInScopeTest = mkCharType", "notInScopeTest :: String -> Data.Data.DataType")
        ]
   in testGroup
        "add signature"
        [ testGroup "signatures are correct" [sigSession (T.unpack $ T.replace "\n" "\\n" def) False False "always" "" (def, Just sig) [] | (def, sig) <- cases]
        , sigSession "exported mode works" False False "exported" "xyz" ("xyz = True", Just "xyz :: Bool") (fst <$> take 3 cases)
        , testGroup
            "diagnostics mode works"
            [ sigSession "with GHC warnings" True True "diagnostics" "" (second Just $ head cases) []
            , sigSession "without GHC warnings" False False "diagnostics" "" (second (const Nothing) $ head cases) []
            ]
        , testSession "keep stale lens" $ do
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

-- | Before ghc9, lists of Char is displayed as [Char], but with ghc9 and up, it's displayed as String
listOfChar :: T.Text
listOfChar | ghcVersion >= GHC90 = "String"
           | otherwise = "[Char]"

-- TODO Replace with lsp-test function when updated to the latest release
getAndResolveCodeLenses :: TextDocumentIdentifier -> Session [CodeLens]
getAndResolveCodeLenses tId = do
    codeLenses <- getCodeLenses tId
    for codeLenses $ \codeLens -> if isJust (codeLens ^. L.data_) then resolveCodeLens codeLens else pure codeLens

-- |Resolves the provided code lens.
resolveCodeLens :: CodeLens -> Session CodeLens
resolveCodeLens cl = do
  rsp <- request SMethod_CodeLensResolve cl
  case rsp ^. L.result of
    Right cl -> return cl
    Left error -> throw (UnexpectedResponseError (SomeLspId $ fromJust $ rsp ^. L.id) error)

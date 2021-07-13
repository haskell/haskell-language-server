{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens
import           Data.Aeson
import qualified Data.Text                as T
import           Ide.Plugin.CallHierarchy
import qualified Language.LSP.Test        as Test
import qualified Language.LSP.Types.Lens  as L
import           System.FilePath
import           Test.Hls

plugin :: PluginDescriptor IdeState
plugin = descriptor "callHierarchy"

main :: IO ()
main = defaultTestRunner $ testGroup "Call Hierarchy" [prepareCallHierarchyTests]

prepareCallHierarchyTests :: TestTree
prepareCallHierarchyTests =
  testGroup
  "Prepare Call Hierarchy"
  [ testCase "variable" $ do
      let contents = T.unlines ["a=3"]
          range = mkRange 0 0 0 3
          selRange = mkRange 0 0 0 1
          expected = mkCallHierarchyItemV "a" SkFunction range selRange
      oneCaseWithCreate contents 0 0 expected
  , testCase "function" $ do
      let contents = T.unlines ["a=(+)"]
          range = mkRange 0 0 0 5
          selRange = mkRange 0 0 0 1
          expected = mkCallHierarchyItemV "a" SkFunction range selRange
      oneCaseWithCreate contents 0 0 expected
  , testCase "datatype" $ do
      let contents = T.unlines ["data A=A"]
          range = mkRange 0 0 0 8
          selRange = mkRange 0 5 0 6
          expected = mkCallHierarchyItemT "A" SkStruct range selRange
      oneCaseWithCreate contents 0 5 expected
  , testCase "data constructor" $ do
      let contents = T.unlines ["data A=A"]
          range = mkRange 0 7 0 8
          selRange = mkRange 0 7 0 8
          expected = mkCallHierarchyItemC "A" SkConstructor range selRange
      oneCaseWithCreate contents 0 7 expected
  , testCase "record" $ do
      let contents = T.unlines ["data A=A{a::Int}"]
          range = mkRange 0 9 0 10
          selRange = mkRange 0 9 0 10
          expected = mkCallHierarchyItemV "a" SkField range selRange
      oneCaseWithCreate contents 0 9 expected
  , testCase "type operator" $ do
      let contents = T.unlines ["{-# LANGUAGE TypeOperators #-}", "type (><)=Maybe"]
          range = mkRange 1 0 1 15
          selRange = mkRange 1 5 1 9
          expected = mkCallHierarchyItemT "><" SkTypeParameter range selRange
      oneCaseWithCreate contents 1 5 expected
  , testCase "type class" $ do
      let contents = T.unlines ["class A a where a :: a -> Int"]
          range = mkRange 0 0 0 29
          selRange = mkRange 0 6 0 7
          expected = mkCallHierarchyItemT "A" SkInterface range selRange
      oneCaseWithCreate contents 0 6 expected
  , testCase "type class method" $ do
      let contents = T.unlines ["class A a where a :: a -> Int"]
          range = mkRange 0 16 0 29
          selRange = mkRange 0 16 0 17
          expected = mkCallHierarchyItemV "a" SkMethod range selRange
      oneCaseWithCreate contents 0 16 expected
  , testCase "type class instance" $ do -- TODO: add details to prevent ambiguous
      let contents = T.unlines ["class A a where", "instance A () where"]
          range = mkRange 1 9 1 10
          selRange = mkRange 1 9 1 10
          expected = mkCallHierarchyItemT "A" SkInterface range selRange
      oneCaseWithCreate contents 1 9 expected
  , testGroup "type family"
      [ testCase "1" $ do
          let contents = T.unlines ["{-# LANGUAGE TypeFamilies #-}", "type family A"]
              range = mkRange 1 0 1 13
              selRange = mkRange 1 12 1 13
              expected = mkCallHierarchyItemT "A" SkFunction range selRange
          oneCaseWithCreate contents 1 12 expected
      , testCase "2" $ do
          let contents = T.unlines ["{-# LANGUAGE TypeFamilies #-}", "type family A a"]
              range = mkRange 1 0 1 15
              selRange = mkRange 1 12 1 13
              expected = mkCallHierarchyItemT "A" SkFunction range selRange
          oneCaseWithCreate contents 1 12 expected
      ]
  , testCase "type family instance" $ do
      let contents = T.unlines
            [ "{-# LANGUAGE TypeFamilies #-}"
            , "type family A a"
            , "type instance A () = ()"
            ]
          range = mkRange 2 14 2 23
          selRange = mkRange 2 14 2 15
          expected = mkCallHierarchyItemT "A" SkInterface range selRange
      oneCaseWithCreate contents 2 14 expected
  -- , testGroup "data family"
  --     [ testCase "1" $ do
  --         let contents = T.unlines ["{-# LANGUAGE TypeFamilies #-}", "data family A"]
  --             range = mkRange 1 0 1 13
  --             selRange = mkRange 1 12 1 13
  --             expected = mkCallHierarchyItem "A" SkFunction range selRange
  --         oneCaseWithCreate contents 1 12 expected
  --     , testCase "2" $ do
  --         let contents = T.unlines [ "{-# LANGUAGE TypeFamilies #-}" , "data family A a"]
  --             range = mkRange 1 0 1 15
  --             selRange = mkRange 1 12 1 13
  --             expected = mkCallHierarchyItem "A" SkFunction range selRange
  --         oneCaseWithCreate contents 1 12 expected
  --     ]
  -- , testCase "data family instance" $ do
  --     let contents = T.unlines
  --           [ "{-# LANGUAGE TypeFamilies #-}"
  --           , "data family A a"
  --           , "data instance A () = A()"
  --           ]
  --         range = mkRange 2 0 2 24
  --         selRange = mkRange 2 14 2 15
  --         expected = mkCallHierarchyItem "A" SkInterface range selRange
  --     oneCaseWithCreate contents 2 14 expected
  , testCase "pattern" $ do
      let contents = T.unlines ["Just x = Just 3"]
          range = mkRange 0 0 0 15
          selRange = mkRange 0 5 0 6
          expected = mkCallHierarchyItemV "x" SkFunction range selRange
      oneCaseWithCreate contents 0 5 expected
  , testCase "pattern with type signature" $ do
      let contents = T.unlines ["{-# LANGUAGE ScopedTypeVariables #-}", "a :: () = ()"]
          range = mkRange 1 0 1 12
          selRange = mkRange 1 0 1 1
          expected = mkCallHierarchyItemV "a" SkFunction range selRange
      oneCaseWithCreate contents 1 0 expected
  , testCase "type synonym" $ do
      let contents = T.unlines ["type A=Bool"]
          range = mkRange 0 0 0 11
          selRange = mkRange 0 5 0 6
          expected = mkCallHierarchyItemT "A" SkTypeParameter range selRange
      oneCaseWithCreate contents 0 5 expected
  , testCase "GADT" $ do
      let contents = T.unlines
            [ "{-# LANGUAGE GADTs #-}"
            , "data A where A :: Int -> A"
            ]
          range = mkRange 1 13 1 26
          selRange = mkRange 1 13 1 14
          expected = mkCallHierarchyItemC "A" SkConstructor range selRange
      oneCaseWithCreate contents 1 13 expected
  ]

oneCaseWithOpen :: FilePath -> Int -> Int -> CallHierarchyItem -> Assertion
oneCaseWithOpen filename queryX queryY expected =
  runSessionWithServer plugin testDataDir $ do
    doc <- openDoc filename "haskell"
    Test.prepareCallHierarchy (mkPrepareCallHierarchyParam doc queryX queryY) >>=
      \case
        [item] -> liftIO $ item @?= expected
        _      -> liftIO $ assertFailure "Not one element"

oneCaseWithCreate :: T.Text -> Int -> Int -> (Uri -> CallHierarchyItem) -> Assertion
oneCaseWithCreate contents queryX queryY expected =
  runSessionWithServer plugin testDataDir $ do
    doc <- createDoc "A.hs" "haskell" contents
    Test.prepareCallHierarchy (mkPrepareCallHierarchyParam doc queryX queryY) >>=
      \case
        [item] -> liftIO $ item @?= expected (doc ^. L.uri)
        res    -> liftIO $ assertFailure "Not one element"

mkCallHierarchyItem :: T.Text -> SymbolKind -> Range -> Range -> Uri -> CallHierarchyItem
mkCallHierarchyItem name kind range selRange uri =
  CallHierarchyItem name kind Nothing Nothing uri range selRange Nothing

mkCallHierarchyItem' :: String -> T.Text -> SymbolKind -> Range -> Range -> Uri -> CallHierarchyItem
mkCallHierarchyItem' prefix name kind range selRange uri =
  CallHierarchyItem name kind Nothing (Just "Main") uri range selRange (Just v)
  where
    v = toJSON $ prefix <> ":" <> T.unpack name <> ":Main:main"

mkCallHierarchyItemC, mkCallHierarchyItemT, mkCallHierarchyItemV ::
  T.Text -> SymbolKind -> Range -> Range -> Uri -> CallHierarchyItem
mkCallHierarchyItemC = mkCallHierarchyItem' "c"
mkCallHierarchyItemT = mkCallHierarchyItem' "t"
mkCallHierarchyItemV = mkCallHierarchyItem' "v"

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

mkPrepareCallHierarchyParam :: TextDocumentIdentifier -> Int -> Int -> CallHierarchyPrepareParams
mkPrepareCallHierarchyParam doc x y = CallHierarchyPrepareParams doc (Position x y) Nothing

mkIncomingCallsParam :: CallHierarchyItem -> CallHierarchyIncomingCallsParams
mkIncomingCallsParam = CallHierarchyIncomingCallsParams Nothing Nothing

mkOutgoingCallsParam :: CallHierarchyItem -> CallHierarchyOutgoingCallsParams
mkOutgoingCallsParam = CallHierarchyOutgoingCallsParams Nothing Nothing

{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}

module Main (main) where

import           Control.Lens             (set, (^.))
import           Control.Monad.Extra
import           Data.Aeson
import           Data.Functor             ((<&>))
import           Data.List                (sort, tails)
import qualified Data.Map                 as M
import qualified Data.Text                as T
import           Ide.Plugin.CallHierarchy
import qualified Language.LSP.Test        as Test
import qualified Language.LSP.Types.Lens  as L
import Development.IDE.Test
import           System.Directory.Extra
import           System.FilePath
import qualified System.IO.Extra
import           Test.Hls
import           Test.Hls.Util            (withCanonicalTempDir)

plugin :: PluginTestDescriptor ()
plugin = mkPluginTestDescriptor' descriptor "call-hierarchy"

main :: IO ()
main = defaultTestRunner $
  testGroup "Call Hierarchy"
    [ prepareCallHierarchyTests
    , incomingCallsTests
    , outgoingCallsTests
    ]

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
--   , testCase "record" $ do
--       let contents = T.unlines ["data A=A{a::Int}"]
--           range = mkRange 0 9 0 10
--           selRange = mkRange 0 9 0 10
--           expected = mkCallHierarchyItemV "a" SkField range selRange
--       oneCaseWithCreate contents 0 9 expected
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
  , testCase "type class instance" $ do
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
  , testGroup "data family"
      [ testCase "1" $ do
          let contents = T.unlines ["{-# LANGUAGE TypeFamilies #-}", "data family A"]
              range = mkRange 1 0 1 11
              selRange = mkRange 1 12 1 13
              expected = mkCallHierarchyItemT "A" SkFunction range selRange
          oneCaseWithCreate contents 1 12 expected
      , testCase "2" $ do
          let contents = T.unlines [ "{-# LANGUAGE TypeFamilies #-}" , "data family A a"]
              range = mkRange 1 0 1 11
              selRange = mkRange 1 12 1 13
              expected = mkCallHierarchyItemT "A" SkFunction range selRange
          oneCaseWithCreate contents 1 12 expected
      ]
  , testCase "data family instance" $ do
      let contents = T.unlines
            [ "{-# LANGUAGE TypeFamilies #-}"
            , "data family A a"
            , "data instance A () = A()"
            ]
          range = mkRange 2 14 2 24
          selRange = mkRange 2 14 2 15
          expected = mkCallHierarchyItemT "A" SkInterface range selRange
      oneCaseWithCreate contents 2 14 expected
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
  , testGroup "type signature"
      [ testCase "next line" $ do
          let contents = T.unlines ["a::Int", "a=3"]
              range = mkRange 1 0 1 3
              selRange = mkRange 1 0 1 1
              expected = mkCallHierarchyItemV "a" SkFunction range selRange
          oneCaseWithCreate contents 0 0 expected
      , testCase "multi functions" $ do
          let contents = T.unlines [ "a,b::Int", "a=3", "b=4"]
              range = mkRange 2 0 2 3
              selRange = mkRange 2 0 2 1
              expected = mkCallHierarchyItemV "b" SkFunction range selRange
          oneCaseWithCreate contents 0 2 expected
      ]
  , testCase "multi pattern" $ do
      let contents = T.unlines
            [ "f (Just _) = ()"
            , "f Nothing = ()"
            ]
          range = mkRange 1 0 1 1
          selRange = mkRange 1 0 1 1
          expected = mkCallHierarchyItemV "f" SkFunction range selRange
      oneCaseWithCreate contents 1 0 expected
  ]

incomingCallsTests :: TestTree
incomingCallsTests =
  testGroup "Incoming Calls"
  [ testGroup "single file"
    [
      testCase "xdata unavailable" $
        runSessionWithServer plugin testDataDir $ do
          doc <- createDoc "A.hs" "haskell" $ T.unlines ["a=3", "b=a"]
          waitForIndex (testDataDir </> "A.hs")
          [item] <- Test.prepareCallHierarchy (mkPrepareCallHierarchyParam doc 1 0)
          let expected = [CallHierarchyIncomingCall item (List [mkRange 1 2 1 3])]
          Test.prepareCallHierarchy (mkPrepareCallHierarchyParam doc 0 0) >>=
            \case
              [item] -> do
                let itemNoData = set L.xdata Nothing item
                Test.incomingCalls (mkIncomingCallsParam itemNoData) >>=
                  \res -> liftIO $ sort expected @=? sort res
              _      -> liftIO $ assertFailure "Not exactly one element"
          closeDoc doc
    , testCase "xdata available" $ do
        let contents = T.unlines ["a=3","b=a"]
            positions = [(1, 0)]
            ranges = [mkRange 1 2 1 3]
        incomingCallTestCase contents 0 1 positions ranges
    , testGroup "data"
      [ testCase "data type" $ do
          let contents = T.unlines ["data A=A"]
              positions = []
              ranges = []
          incomingCallTestCase contents 0 5 positions ranges
      , testCase "data constructor" $ do
          let contents = T.unlines ["data A=A"]
              positions = [(0, 5)]
              ranges = [mkRange 0 7 0 8]
          incomingCallTestCase contents 0 7 positions ranges
    --   , testCase "record" $ do
    --       let contents = T.unlines ["data A=A{a::Int}"]
    --           positions = [(0, 5), (0, 7)]
    --           ranges = [mkRange 0 9 0 10, mkRange 0 9 0 10]
    --       incomingCallTestCase contents 0 9 positions ranges
      ]
    , testCase "function" $ do
        let contents = T.unlines ["a=(+)"]
            positions = [(0, 0)]
            ranges = [mkRange 0 2 0 5]
        incomingCallTestCase contents 0 3 positions ranges
    , testCase "type operator" $ do
        let contents = T.unlines
              [ "{-# LANGUAGE TypeOperators #-}"
              , "type (><)=Int"]
            positions = [(1, 5)]
            ranges = [mkRange 1 10 1 13]
        incomingCallTestCase contents 1 10 positions ranges
    , testGroup "type class"
      [ testCase "type class method" $ do
          let contents = T.unlines ["class A a where a :: a -> Int"]
              positions = [(0, 6)]
              ranges = [mkRange 0 16 0 17]
          incomingCallTestCase contents 0 16 positions ranges
      , testCase "type class instance" $ do
          let contents = T.unlines
                [ "class A a where a :: a -> Int"
                , "instance A () where a = const 3"]
              positions = [(0, 6)]
              ranges = [mkRange 0 16 0 17]
          incomingCallTestCase contents 1 20 positions ranges
      , testCase "goto typeclass instance" $ do
          let contents = T.unlines
                [ "class F a where f :: a"
                , "instance F Bool where f = x"
                , "instance F Int where f = 3"
                , "x = True"
                ]
              positions = [(1, 22)]
              ranges = [mkRange 1 26 1 27]
          incomingCallTestCase contents 3 0 positions ranges
      ]
    , testCase "type family instance" $ do
        let contents = T.unlines
              [ "{-# LANGUAGE TypeFamilies #-}"
              , "type family A a"
              , "type instance A Int = Char"
              ]
            positions = [(2, 14)]
            ranges = [mkRange 2 22 2 26]
        incomingCallTestCase contents 2 22 positions ranges
    , testCase "GADT" $ do
        let contents = T.unlines
              [ "{-# LANGUAGE GADTs #-}"
              , "data A where B :: Int -> A"
              ]
            positions = [(1, 5)]
            ranges = [mkRange 1 13 1 14]
        incomingCallTestCase contents 1 13 positions ranges
    , testCase "multi pattern" $ do
        let contents = T.unlines
                [ "f 1 = 1"
                , "f 2 = 2"
                , "g = f"
                ]
            positions = [(2, 0)]
            ranges = [mkRange 2 4 2 5]
        incomingCallTestCase contents 1 0 positions ranges
    ]
  , testGroup "multi file"
    [ testCase "1" $ do
        let mp = M.fromList [
                  ("A.hs", [ ((5, 0), mkRange 5 7 5 11)
                           , ((6, 0), mkRange 6 7 6 11)
                           , ((8, 0), mkRange 9 25 9 29)
                           ]
                  )]
        incomingCallMultiFileTestCase "A.hs" 4 0 mp
    , testCase "2" $ do
        let mp = M.fromList [
                  ("A.hs", [ ((4, 0), mkRange 4 13 4 16)
                           , ((8, 0), mkRange 10 7 10 10)
                           ]
                  )
                , ("B.hs", [ ((4, 0), mkRange 4 8 4 11)])
                ]
        incomingCallMultiFileTestCase "C.hs" 2 0 mp
    ]
  ]

outgoingCallsTests :: TestTree
outgoingCallsTests =
  testGroup "Outgoing Calls"
  [ testGroup "single file"
    [
      testCase "xdata unavailable" $ withCanonicalTempDir $ \dir ->
        runSessionWithServer plugin dir $ do
          doc <- createDoc "A.hs" "haskell" $ T.unlines ["a=3", "b=a"]
          waitForIndex (dir </> "A.hs")
          [item] <- Test.prepareCallHierarchy (mkPrepareCallHierarchyParam doc 0 1)
          let expected = [CallHierarchyOutgoingCall item (List [mkRange 1 2 1 3])]
          Test.prepareCallHierarchy (mkPrepareCallHierarchyParam doc 1 0) >>=
            \case
              [item] -> do
                let itemNoData = set L.xdata Nothing item
                Test.outgoingCalls (mkOutgoingCallsParam itemNoData) >>=
                  \res -> liftIO $ sort expected @=? sort res
              _      -> liftIO $ assertFailure "Not exactly one element"
          closeDoc doc
    , testCase "xdata available" $ do
        let contents = T.unlines ["a=3", "b=a"]
            positions = [(0, 0)]
            ranges = [mkRange 1 2 1 3]
        outgoingCallTestCase contents 1 0 positions ranges
    , testGroup "data"
      [ testCase "data type" $ do
          let contents = T.unlines ["data A=A"]
              positions = [(0, 7)]
              ranges = [mkRange 0 7 0 8]
          outgoingCallTestCase contents 0 5 positions ranges
      , testCase "data constructor" $ do
          let contents = T.unlines ["data A=A"]
              positions = []
              ranges = []
          outgoingCallTestCase contents 0 7 positions ranges
    --   , testCase "record" $ do
    --       let contents = T.unlines ["data A=A{a::Int}"]
    --           positions = [(0, 7), (0, 9)]
    --           ranges = [mkRange 0 7 0 8, mkRange 0 9 0 10]
    --       outgoingCallTestCase contents 0 5 positions ranges
      ]
      , testCase "function" $ do
          let contents = T.unlines ["a=3", "b=4", "c=a+b"]
              positions = [(0, 1), (1, 1)]
              ranges = [mkRange 2 2 2 3, mkRange 2 4 2 5]
          outgoingCallTestCase contents 2 0 positions ranges
      , testCase "type synonym" $ do
          let contents = T.unlines ["data A", "type B=A"]
              positions = [(0, 5)]
              ranges = [mkRange 1 7 1 8]
          outgoingCallTestCase contents 1 5 positions ranges
      , testCase "type class instance" $ do
          let contents = T.unlines
                [ "class A a where a :: a"
                , "instance A () where a = ()"
                ]
              positions = [(0, 16)]
              ranges = [mkRange 0 16 0 17]
          outgoingCallTestCase contents 1 9 positions ranges
      , testCase "data family instance" $ do
          let contents = T.unlines
                [ "{-# LANGUAGE TypeFamilies #-}"
                , "data family A a"
                , "data instance A () = B"
                ]
              positions = [(2, 21)]
              ranges = [mkRange 2 21 2 22]
          outgoingCallTestCase contents 1 12 positions ranges
      , testCase "GADT" $ do
          let contents = T.unlines ["{-# LANGUAGE GADTs #-}", "data A where B :: A"]
              positions = [(1, 13)]
              ranges = [mkRange 1 13 1 14]
          outgoingCallTestCase contents 1 5 positions ranges
    ]
  , testGroup "multi file"
    [ testCase "1" $ do
        let mp = M.fromList [
                  ("A.hs", [ ((4, 0), mkRange 5 7 5 11)])
                , ("B.hs", [ ((4, 0), mkRange 5 14 5 17)])
                , ("C.hs", [ ((3, 0), mkRange 5 20 5 23)])
                ]
        outgoingCallMultiFileTestCase "A.hs" 5 0 mp
    , testCase "2" $ do
        let mp = M.fromList [
                  ("A.hs", [ ((4, 0), mkRange 9 25 9 29)
                           , ((5, 0), mkRange 10 25 10 29)
                           ]
                  )
                , ("B.hs", [ ((2, 9), mkRange 9 2 9 3)
                           , ((2, 13), mkRange 10 2 10 3)
                           , ((4, 0), mkRange 9 7 9 10)
                           , ((5, 0), mkRange 9 13 9 16)
                           , ((6, 0), mkRange 9 19 9 22)
                           ]
                  )
                , ("C.hs", [ ((2, 0), mkRange 10 7 10 10)
                           , ((3, 0), mkRange 10 13 10 16)
                           , ((4, 0), mkRange 10 19 10 22)
                           ]
                  )
                ]
        outgoingCallMultiFileTestCase "A.hs" 8 0 mp
    ]
  ]

deriving instance Ord CallHierarchyIncomingCall
deriving instance Ord CallHierarchyOutgoingCall

incomingCallTestCase :: T.Text -> Int -> Int -> [(Int, Int)] -> [Range] -> Assertion
incomingCallTestCase contents queryX queryY positions ranges = withCanonicalTempDir $ \dir ->
  runSessionWithServer plugin dir $ do
    doc <- createDoc "A.hs" "haskell" contents
    waitForIndex (dir </> "A.hs")
    items <- concatMapM (\((x, y), range) ->
      Test.prepareCallHierarchy (mkPrepareCallHierarchyParam doc x y)
          <&> map (, range)
      )
      (zip positions ranges)
    let expected = map mkCallHierarchyIncomingCall items
    -- liftIO delay
    Test.prepareCallHierarchy (mkPrepareCallHierarchyParam doc queryX queryY) >>=
      \case
        [item] -> do
          Test.incomingCalls (mkIncomingCallsParam item) >>=
                    \res -> liftIO $ sort expected @=? sort res
        _      -> liftIO $ assertFailure "Not one element"
    closeDoc doc

incomingCallMultiFileTestCase :: FilePath -> Int -> Int -> M.Map FilePath [((Int, Int), Range)] -> Assertion
incomingCallMultiFileTestCase filepath queryX queryY mp =
  runSessionWithServer plugin testDataDir $ do
    doc <- openDoc filepath "haskell"
    waitForIndex (testDataDir </> filepath)
    items <- fmap concat $ sequence $ M.elems $ M.mapWithKey (\fp pr -> do
              p <- openDoc fp "haskell"
              waitForKickDone
              concatMapM (\((x, y), range) ->
                  Test.prepareCallHierarchy (mkPrepareCallHierarchyParam p x y)
                    <&> map (, range)
                ) pr) mp
    let expected = map mkCallHierarchyIncomingCall items
    -- liftIO delay
    Test.prepareCallHierarchy (mkPrepareCallHierarchyParam doc queryX queryY) >>=
      \case
        [item] -> do
          Test.incomingCalls (mkIncomingCallsParam item) >>=
                    \res -> liftIO $ sort expected @=? sort res
        _      -> liftIO $ assertFailure "Not one element"
    closeDoc doc

outgoingCallTestCase :: T.Text -> Int -> Int -> [(Int, Int)] -> [Range] -> Assertion
outgoingCallTestCase contents queryX queryY positions ranges = withCanonicalTempDir $ \dir ->
  runSessionWithServer plugin dir $ do
    doc <- createDoc "A.hs" "haskell" contents
    waitForIndex (dir </> "A.hs")
    items <- concatMapM (\((x, y), range) ->
      Test.prepareCallHierarchy (mkPrepareCallHierarchyParam doc x y)
          <&> map (, range)
      )
      (zip positions ranges)
    let expected = map mkCallHierarchyOutgoingCall items
    Test.prepareCallHierarchy (mkPrepareCallHierarchyParam doc queryX queryY) >>=
      \case
        [item] -> do
          Test.outgoingCalls (mkOutgoingCallsParam item) >>=
                    \res -> liftIO $ sort expected @=? sort res
        _      -> liftIO $ assertFailure "Not one element"
    closeDoc doc

outgoingCallMultiFileTestCase :: FilePath -> Int -> Int -> M.Map FilePath [((Int, Int), Range)] -> Assertion
outgoingCallMultiFileTestCase filepath queryX queryY mp =
  runSessionWithServer plugin testDataDir $ do
    doc <- openDoc filepath "haskell"
    waitForIndex (testDataDir </> filepath)
    items <- fmap concat $ sequence $ M.elems $ M.mapWithKey (\fp pr -> do
              p <- openDoc fp "haskell"
              waitForKickDone
              concatMapM (\((x, y), range) ->
                  Test.prepareCallHierarchy (mkPrepareCallHierarchyParam p x y)
                    <&> map (, range)
                ) pr) mp
    let expected = map mkCallHierarchyOutgoingCall items
    Test.prepareCallHierarchy (mkPrepareCallHierarchyParam doc queryX queryY) >>=
      \case
        [item] -> do
          Test.outgoingCalls (mkOutgoingCallsParam item) >>=
                    \res -> liftIO $ sort expected @=? sort res
        _      -> liftIO $ assertFailure "Not one element"
    closeDoc doc

oneCaseWithCreate :: T.Text -> Int -> Int -> (Uri -> CallHierarchyItem) -> Assertion
oneCaseWithCreate contents queryX queryY expected = withCanonicalTempDir $ \dir ->
  runSessionWithServer plugin dir $ do
    doc <- createDoc "A.hs" "haskell" contents
    waitForIndex (dir </> "A.hs")
    Test.prepareCallHierarchy (mkPrepareCallHierarchyParam doc queryX queryY) >>=
      \case
        [item] -> liftIO $ item @?= expected (doc ^. L.uri)
        res    -> liftIO $ assertFailure "Not one element"
    closeDoc doc

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

mkCallHierarchyIncomingCall :: (CallHierarchyItem, Range) -> CallHierarchyIncomingCall
mkCallHierarchyIncomingCall (item, range) = CallHierarchyIncomingCall item (List [range])

mkCallHierarchyOutgoingCall :: (CallHierarchyItem, Range) -> CallHierarchyOutgoingCall
mkCallHierarchyOutgoingCall (item, range) = CallHierarchyOutgoingCall item (List [range])

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

mkPrepareCallHierarchyParam :: TextDocumentIdentifier -> Int -> Int -> CallHierarchyPrepareParams
mkPrepareCallHierarchyParam doc x y = CallHierarchyPrepareParams doc (Position (fromIntegral x) (fromIntegral y)) Nothing

mkIncomingCallsParam :: CallHierarchyItem -> CallHierarchyIncomingCallsParams
mkIncomingCallsParam = CallHierarchyIncomingCallsParams Nothing Nothing

mkOutgoingCallsParam :: CallHierarchyItem -> CallHierarchyOutgoingCallsParams
mkOutgoingCallsParam = CallHierarchyOutgoingCallsParams Nothing Nothing

-- Wait for a special test message emitted by ghcide when a file is indexed,
-- so that call hierarchy can safely query the database.
waitForIndex :: FilePath -> Session ()
waitForIndex fp1 = skipManyTill anyMessage $ void $ referenceReady lenientEquals
  where
    -- fp1 may be relative, in that case we check that it is a suffix of the
    -- filepath from the message
    lenientEquals :: FilePath -> Bool
    lenientEquals fp2
      | isRelative fp1 = any (equalFilePath fp1) (map (foldr (</>) "") $ tails $ splitDirectories fp2)
      | otherwise = equalFilePath fp1 fp2


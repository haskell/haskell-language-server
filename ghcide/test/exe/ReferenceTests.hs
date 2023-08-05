
module ReferenceTests (tests) where

import           Control.Applicative.Combinators
import qualified Control.Lens                    as Lens
import           Control.Monad
import           Control.Monad.IO.Class          (liftIO)
import           Data.List.Extra
import qualified Data.Set                        as Set
import           Development.IDE.Test            (configureCheckProject,
                                                  referenceReady)
import           Development.IDE.Types.Location
import qualified Language.LSP.Protocol.Lens      as L
import           Language.LSP.Protocol.Types     hiding
                                                 (SemanticTokenAbsolute (..),
                                                  SemanticTokenRelative (..),
                                                  SemanticTokensEdit (..),
                                                  mkRange)
import           Language.LSP.Test
import           System.Directory
import           System.FilePath
-- import Test.QuickCheck.Instances ()
import           Control.Lens                    ((^.))
import           Data.Tuple.Extra
import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.HUnit
import           TestUtils


tests :: TestTree
tests = testGroup "references"
    [ testGroup "can get references to FOIs"
          [ referenceTest "can get references to symbols"
                          ("References.hs", 4, 7)
                          YesIncludeDeclaration
                          [ ("References.hs", 4, 6)
                          , ("References.hs", 6, 0)
                          , ("References.hs", 6, 14)
                          , ("References.hs", 9, 7)
                          , ("References.hs", 10, 11)
                          ]

          , referenceTest "can get references to data constructor"
                          ("References.hs", 13, 2)
                          YesIncludeDeclaration
                          [ ("References.hs", 13, 2)
                          , ("References.hs", 16, 14)
                          , ("References.hs", 19, 21)
                          ]

          , referenceTest "getting references works in the other module"
                          ("OtherModule.hs", 6, 0)
                          YesIncludeDeclaration
                          [ ("OtherModule.hs", 6, 0)
                          , ("OtherModule.hs", 8, 16)
                          ]

          , referenceTest "getting references works in the Main module"
                          ("Main.hs", 9, 0)
                          YesIncludeDeclaration
                          [ ("Main.hs", 9, 0)
                          , ("Main.hs", 10, 4)
                          ]

          , referenceTest "getting references to main works"
                          ("Main.hs", 5, 0)
                          YesIncludeDeclaration
                          [ ("Main.hs", 4, 0)
                          , ("Main.hs", 5, 0)
                          ]

          , referenceTest "can get type references"
                          ("Main.hs", 9, 9)
                          YesIncludeDeclaration
                          [ ("Main.hs", 9, 0)
                          , ("Main.hs", 9, 9)
                          , ("Main.hs", 10, 0)
                          ]

          , expectFailBecause "references provider does not respect includeDeclaration parameter" $
 referenceTest "works when we ask to exclude declarations"
                          ("References.hs", 4, 7)
                          NoExcludeDeclaration
                          [ ("References.hs", 6, 0)
                          , ("References.hs", 6, 14)
                          , ("References.hs", 9, 7)
                          , ("References.hs", 10, 11)
                          ]

          , referenceTest "INCORRECTLY returns declarations when we ask to exclude them"
                          ("References.hs", 4, 7)
                          NoExcludeDeclaration
                          [ ("References.hs", 4, 6)
                          , ("References.hs", 6, 0)
                          , ("References.hs", 6, 14)
                          , ("References.hs", 9, 7)
                          , ("References.hs", 10, 11)
                          ]
          ]

    , testGroup "can get references to non FOIs"
          [ referenceTest "can get references to symbol defined in a module we import"
                          ("References.hs", 22, 4)
                          YesIncludeDeclaration
                          [ ("References.hs", 22, 4)
                          , ("OtherModule.hs", 0, 20)
                          , ("OtherModule.hs", 4, 0)
                          ]

          , referenceTest "can get references in modules that import us to symbols we define"
                          ("OtherModule.hs", 4, 0)
                          YesIncludeDeclaration
                          [ ("References.hs", 22, 4)
                          , ("OtherModule.hs", 0, 20)
                          , ("OtherModule.hs", 4, 0)
                          ]

          , referenceTest "can get references to symbol defined in a module we import transitively"
                          ("References.hs", 24, 4)
                          YesIncludeDeclaration
                          [ ("References.hs", 24, 4)
                          , ("OtherModule.hs", 0, 48)
                          , ("OtherOtherModule.hs", 2, 0)
                          ]

          , referenceTest "can get references in modules that import us transitively to symbols we define"
                          ("OtherOtherModule.hs", 2, 0)
                          YesIncludeDeclaration
                          [ ("References.hs", 24, 4)
                          , ("OtherModule.hs", 0, 48)
                          , ("OtherOtherModule.hs", 2, 0)
                          ]

          , referenceTest "can get type references to other modules"
                          ("Main.hs", 12, 10)
                          YesIncludeDeclaration
                          [ ("Main.hs", 12, 7)
                          , ("Main.hs", 13, 0)
                          , ("References.hs", 12, 5)
                          , ("References.hs", 16, 0)
                          ]
          ]
    ]

-- | When we ask for all references to symbol "foo", should the declaration "foo
-- = 2" be among the references returned?
data IncludeDeclaration =
    YesIncludeDeclaration
    | NoExcludeDeclaration

getReferences' :: SymbolLocation -> IncludeDeclaration -> Session ([Location])
getReferences' (file, l, c) includeDeclaration = do
    doc <- openDoc file "haskell"
    getReferences doc (Position l c) $ toBool includeDeclaration
    where toBool YesIncludeDeclaration = True
          toBool NoExcludeDeclaration  = False

referenceTestSession :: String -> FilePath -> [FilePath] -> (FilePath -> Session ()) -> TestTree
referenceTestSession name thisDoc docs' f = testSessionWithExtraFiles "references" name $ \dir -> do
  -- needed to build whole project indexing
  configureCheckProject True
  let docs = map (dir </>) $ delete thisDoc $ nubOrd docs'
  -- Initial Index
  docid <- openDoc thisDoc "haskell"
  let
    loop :: [FilePath] -> Session ()
    loop [] = pure ()
    loop docs = do
      doc <- skipManyTill anyMessage $ referenceReady (`elem` docs)
      loop (delete doc docs)
  loop docs
  f dir
  closeDoc docid

-- | Given a location, lookup the symbol and all references to it. Make sure
-- they are the ones we expect.
referenceTest :: String -> SymbolLocation -> IncludeDeclaration -> [SymbolLocation] -> TestTree
referenceTest name loc includeDeclaration expected =
    referenceTestSession name (fst3 loc) docs $ \dir -> do
        actual <- getReferences' loc includeDeclaration
        liftIO $ actual `expectSameLocations` map (first3 (dir </>)) expected
  where
    docs = map fst3 expected

type SymbolLocation = (FilePath, UInt, UInt)

expectSameLocations :: [Location] -> [SymbolLocation] -> Assertion
expectSameLocations actual expected = do
    let actual' =
            Set.map (\location -> (location ^. L.uri
                                   , location ^. L.range . L.start . L.line . Lens.to fromIntegral
                                   , location ^. L.range . L.start . L.character . Lens.to fromIntegral))
            $ Set.fromList actual
    expected' <- Set.fromList <$>
        (forM expected $ \(file, l, c) -> do
                              fp <- canonicalizePath file
                              return (filePathToUri fp, l, c))
    actual' @?= expected'

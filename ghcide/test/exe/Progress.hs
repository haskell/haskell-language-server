{-# LANGUAGE PackageImports #-}
module Progress (tests) where

import           Control.Concurrent.STM
import           Data.Foldable                          (for_)
import qualified Data.HashMap.Strict                    as Map
import           Development.IDE                        (NormalizedFilePath)
import           Development.IDE.Core.ProgressReporting
import           Development.IDE.Types.Path
import qualified "list-t" ListT
import qualified StmContainers.Map                      as STM
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Progress"
    [ reportProgressTests
    ]

data InProgressModel = InProgressModel {
    done, todo :: Int,
    current    :: Map.HashMap (Path Abs NormalizedFilePath) Int
}

reportProgressTests :: TestTree
reportProgressTests = testGroup "recordProgress"
    [ test "addNew"   addNew
    , test "increase" increase
    , test "decrease" decrease
    , test "done" done
    ]
    where
        p0 = pure $ InProgressModel 0 0 mempty
        addNew = recordProgressModel (mkAbsPath "A") succ p0
        increase = recordProgressModel (mkAbsPath "A") succ addNew
        decrease = recordProgressModel (mkAbsPath "A") succ increase
        done = recordProgressModel (mkAbsPath "A") pred decrease
        recordProgressModel key change state =
            model state $ \st -> recordProgress st key change
        model stateModelIO k = do
            state <- fromModel =<< stateModelIO
            _ <- k state
            toModel state
        test name p = testCase name $ do
            InProgressModel{..} <- p
            (done, todo) @?= (length (filter (==0) (Map.elems current)), Map.size current)

fromModel :: InProgressModel -> IO InProgressState
fromModel InProgressModel{..} = do
    doneVar <- newTVarIO done
    todoVar <- newTVarIO todo
    currentVar <- STM.newIO
    atomically $ for_ (Map.toList current) $ \(k,v) -> STM.insert v k currentVar
    return InProgressState{..}

toModel :: InProgressState -> IO InProgressModel
toModel InProgressState{..} = atomically $ do
    done <- readTVar doneVar
    todo <- readTVar todoVar
    current <- Map.fromList <$> ListT.toList (STM.listT currentVar)
    return InProgressModel{..}

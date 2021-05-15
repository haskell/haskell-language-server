module Progress (tests) where

import qualified Data.HashMap.Strict                    as Map
import           Development.IDE.Core.ProgressReporting
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Progress"
    [ reportProgressTests
    ]

reportProgressTests :: TestTree
reportProgressTests = testGroup "recordProgress"
    [ test "addNew"   addNew
    , test "increase" increase
    , test "decrease" decrease
    , test "done" done
    ]
    where
        p0 = InProgress 0 0 mempty
        addNew = recordProgress "A" succ p0
        increase = recordProgress "A" succ addNew
        decrease = recordProgress "A" succ increase
        done = recordProgress "A" pred decrease
        model InProgress{..} =
            (done, todo) @?= (length (filter (==0) (Map.elems current)), Map.size current)
        test name p = testCase name $ model p

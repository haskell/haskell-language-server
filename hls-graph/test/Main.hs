import qualified Spec
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.Ingredients.Rerun (defaultMainWithRerun)
import           Test.Tasty.Runners           (NumThreads (..))

main :: IO ()
main = testSpecs Spec.spec >>= defaultMainWithRerun . localOption (NumThreads 1) . testGroup "tactics"

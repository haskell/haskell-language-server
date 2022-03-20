import qualified Spec
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.Ingredients.Rerun (defaultMainWithRerun)

main :: IO ()
main = testSpecs Spec.spec >>= defaultMainWithRerun . testGroup "tactics"

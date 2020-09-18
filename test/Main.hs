import Test.Hspec
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import App hiding (main)
import Model

main :: IO ()
main = check prop_ListReverses

prop_ListReverses :: Property
prop_ListReverses = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
  reverse (reverse xs) === xs
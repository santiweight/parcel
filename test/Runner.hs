
import Prelude
import qualified Test.Parcel
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "all" tests
  where
    tests = [Test.Parcel.tests]

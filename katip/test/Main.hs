module Main (main) where

-------------------------------------------------------------------------------
import           Test.Tasty
-------------------------------------------------------------------------------
import qualified Katip.Tests
import qualified Katip.Tests.Format.Time
import qualified Katip.Tests.Scribes.Handle
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite = testGroup "katip"
  [
    Katip.Tests.tests
  , Katip.Tests.Format.Time.tests
  , Katip.Tests.Scribes.Handle.tests
  ]

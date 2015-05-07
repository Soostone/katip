module Main (main) where

-------------------------------------------------------------------------------
import           Test.Tasty
-------------------------------------------------------------------------------
import           Katip.Tests
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite = testGroup "katip"
  [
    Katip.Tests.tests
  ]

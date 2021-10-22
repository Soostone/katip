module Main (main) where

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
import qualified Katip.Tests
import qualified Katip.Tests.Format.Time
import qualified Katip.Tests.Scribes.Handle
import Test.Tasty

-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite =
  testGroup
    "katip"
    [ Katip.Tests.tests,
      Katip.Tests.Format.Time.tests,
      Katip.Tests.Scribes.Handle.tests
    ]

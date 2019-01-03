module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Test.Tasty
-------------------------------------------------------------------------------
import qualified Katip.Tests.Scribes.Datadog.TCP
-------------------------------------------------------------------------------



main :: IO ()
main = defaultMain tests


-------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "katip-datadog"
  [ Katip.Tests.Scribes.Datadog.TCP.tests
  ]

module Main
  ( main,
  )
where

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
import qualified Katip.Tests.Scribes.Datadog.TCP
import Test.Tasty

-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

-------------------------------------------------------------------------------
tests :: TestTree
tests =
  testGroup
    "katip-datadog"
    [ Katip.Tests.Scribes.Datadog.TCP.tests
    ]

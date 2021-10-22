module Main
  ( main,
  )
where

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
import qualified Katip.Tests.Scribes.LogzIO.HTTPS
import Test.Tasty

-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

-------------------------------------------------------------------------------
tests :: TestTree
tests =
  testGroup
    "katip-logzio"
    [ Katip.Tests.Scribes.LogzIO.HTTPS.tests
    ]

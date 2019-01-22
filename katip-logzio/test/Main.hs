module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Test.Tasty
-------------------------------------------------------------------------------
import qualified Katip.Tests.Scribes.LogzIO.HTTPS
-------------------------------------------------------------------------------



main :: IO ()
main = defaultMain tests


-------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "katip-logzio"
  [ Katip.Tests.Scribes.LogzIO.HTTPS.tests
  ]

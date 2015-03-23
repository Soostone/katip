{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Map                    as M
import           Data.Monoid
import           Database.Bloodhound
import           Network.HTTP.Client         (defaultManagerSettings)
import           Test.Tasty
import           Test.Tasty.HUnit
-------------------------------------------------------------------------------
import           Katip
import           Katip.Scribes.ElasticSearch
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain $ testGroup "katip-elasticsearch"
  [
    withResource setupSearch teardownSearch esTests
  ]


-------------------------------------------------------------------------------
setupSearch :: IO (Scribe, IO ())
setupSearch = do
    bh recreateESSchema
    mkEsScribe defaultEsScribeCfg svr ixn mn DebugS V3


-------------------------------------------------------------------------------
teardownSearch :: (Scribe, IO ()) -> IO ()
teardownSearch (_, finalizer) = do
  finalizer
  bh dropESSchema


-------------------------------------------------------------------------------
esTests :: IO (Scribe, IO ()) -> TestTree
esTests setup = testGroup "elasticsearch scribe"
  [
    testCase "it works" $ withTestLogging setup $ \done -> do
       $(logT) () mempty InfoS "A test message"
       liftIO $ do
         done
         print =<< getLogs
         True @?= False
  ]


-------------------------------------------------------------------------------
getLogs = bh $ searchByIndex ixn $ mkSearch Nothing Nothing


-------------------------------------------------------------------------------
bh = withBH defaultManagerSettings svr


-------------------------------------------------------------------------------
withTestLogging setup f = do
    (scribe, done) <- setup
    le <- initLogEnv ns env
    runKatipT le { _logEnvScribes = M.singleton "es" scribe} (f done)
  where
    ns = Namespace ["katip-test"]
    env = Environment "test"

-------------------------------------------------------------------------------
svr = Server "http://localhost:9200"
ixn = IndexName "katip-elasticsearch-tests"
ixs = defaultIndexSettings
mn = MappingName "logs"
mapping = () -- ehhh


-------------------------------------------------------------------------------
recreateESSchema = dropESSchema >> createESSchema


-------------------------------------------------------------------------------
dropESSchema = void $ deleteIndex ixn


-------------------------------------------------------------------------------
createESSchema = do
  createIndex ixs ixn
  putMapping ixn mn mapping

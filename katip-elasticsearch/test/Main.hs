{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Lens                hiding (mapping)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.Map                    as M
import           Data.Monoid
import           Database.Bloodhound         hiding (key)
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
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
    testCase "it flushes to elasticsearch" $ withTestLogging setup $ \done -> do
       $(logT) () mempty InfoS "A test message"
       liftIO $ do
         done
         logs <- getLogs
         length logs @?= 1
         let l = head logs
         l ^? key "_source" . key "msg" . _String @?= Just "A test message"
  ]


-------------------------------------------------------------------------------
getLogs :: IO [Value]
getLogs = do
  r <- bh $ searchByIndex ixn $ mkSearch Nothing Nothing
  statusCode (responseStatus r) @?= 200
  return $ responseBody r ^.. key "hits" . key "hits" . values


-------------------------------------------------------------------------------
bh :: BH IO a -> IO a
bh = withBH defaultManagerSettings svr


-------------------------------------------------------------------------------
withTestLogging
  :: IO (Scribe, IO a) -> (IO Reply -> KatipT IO b) -> IO b
withTestLogging setup f = do
    (scr, done) <- setup
    le <- initLogEnv ns env
    let done' = done >> bh (refreshIndex ixn)
    runKatipT le { _logEnvScribes = M.singleton "es" scr} (f done')
  where
    ns = Namespace ["katip-test"]
    env = Environment "test"


-------------------------------------------------------------------------------
svr :: Server
svr = Server "http://localhost:9200"


-------------------------------------------------------------------------------
ixn :: IndexName
ixn = IndexName "katip-elasticsearch-tests"


-------------------------------------------------------------------------------
ixs :: IndexSettings
ixs = defaultIndexSettings

-------------------------------------------------------------------------------
mn :: MappingName
mn = MappingName "logs"


-------------------------------------------------------------------------------
mapping :: ()
mapping = () -- ehhh


-------------------------------------------------------------------------------
recreateESSchema :: BH IO Reply
recreateESSchema = dropESSchema >> createESSchema


-------------------------------------------------------------------------------
dropESSchema :: BH IO ()
dropESSchema = void $ deleteIndex ixn


-------------------------------------------------------------------------------
createESSchema :: BH IO Reply
createESSchema = do
  createIndex ixs ixn
  putMapping ixn mn mapping

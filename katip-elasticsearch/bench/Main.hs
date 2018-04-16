{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
    ( main
    ) where

import           Control.Concurrent (threadDelay)
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Criterion.Main
import           Data.Aeson
import qualified Data.HashMap.Strict                  as HM
import           Data.IORef
import           Data.Monoid
import           Data.Proxy                           (Proxy (..))
import           System.Random
import qualified Data.Text                            as T
import           Data.Time.Clock
import           Database.V1.Bloodhound.Types
import qualified Database.V5.Bloodhound.Client        as V5
import qualified Database.V5.Bloodhound.Types         as V5
import qualified Network.HTTP.Client                  as HTTP
import           Numeric

import           Katip
import           Katip.Scribes.ElasticSearch
import           Katip.Scribes.ElasticSearch.Internal (ESV1)

main :: IO ()
main = do
  docCounter <- newIORef 0
  -- rng <- mkRNG
  defaultMain
    [
    --   mkDocIdBenchmark rng
    -- , deannotateValueBenchmark
    esLoggingBenchmark docCounter
    ]
  count <- readIORef docCounter
  putStrLn $ "Doc inserted count was: " <> show count

-- mkDocIdBenchmark :: RNG -> Benchmark
-- mkDocIdBenchmark rng = bgroup "mkDocId"
--   [
--     bench "mkDocId (randomIO)" $ nfIO (mkDocId (Proxy :: Proxy ESV1))
--   , bench "mkDocId' (shared)" $ nfIO $ mkDocId' rng
--   ]

-- deannotateValueBenchmark :: Benchmark
-- deannotateValueBenchmark = bgroup "deannotateValue"
--  [
--    bench "deannotateValue" $ nf deannotateValue annotatedValue
--  ]


-- annotatedValue :: Value
-- annotatedValue = Object $ HM.fromList [ ("a::string", String "whatever")
--                                       , ("b::double", Number 4.5)
--                                       , ("c::long", Number 4)
--                                       , ("d::boolean", Bool True)
--                                       , ("e::null", Null)
--                                       ]

-- mkDocId' :: RNG -> IO DocId
-- mkDocId' rng = do
--     is <- withRNG rng $ \gen -> replicateM len $ mk gen
--     return . DocId . T.pack . concatMap (`showHex` "") $ is

deriving instance NFData DocId

esLoggingBenchmark :: IORef Integer -> Benchmark
esLoggingBenchmark docCounter = bgroup "ES logging"
 [
   -- bench "log 10 messages" $ nfIO (logMessages docCounter 10)
 -- , bench "log 100 messages" $ nfIO (logMessages docCounter 100)
 -- , bench "log 1000 Messages" $ nfIO (logMessages docCounter 1000)
 -- , bench "log 10000 Messages" $ nfIO (logMessages docCounter 10000)
 -- , bench "log 50000 Messages" $ nfIO (logMessages docCounter 50000)

 -- , bench "bulk log 10 messages" $ nfIO (logMessagesBulk docCounter 10)
 -- , bench "bulk log 100 messages" $ nfIO (logMessagesBulk docCounter 100)
 -- , bench "bulk log 1000 Messages" $ nfIO (logMessagesBulk docCounter 1000)
   bench "bulk log 10000 Messages" $ nfIO (logMessagesBulk docCounter 10000)
 , bench "bulk log 50000 Messages" $ nfIO (logMessagesBulk docCounter 50000)
 , bench "bulk log 1000000 Messages"
   $ nfIO (logMessagesBulk docCounter 1000000)
 ]

logMessages :: IORef Integer -> Int -> IO ()
logMessages docCounter repeats = do
  (_, mkLogEnv) <- mkEsBenchLogEnv
  bracket mkLogEnv closeScribes
    $ \ le -> forM_ [1..repeats] $ \i -> runKatipT le $ do
      liftIO $ atomicModifyIORef' docCounter (\x -> (x+1, ()))
      logMsg "ns" InfoS ("This goes to elasticsearch: "
                         <> (logStr $ T.pack $ show i))

mkEsBenchLogEnv :: IO (IO Reply, IO LogEnv)
mkEsBenchLogEnv = do
  connManager <- HTTP.newManager HTTP.defaultManagerSettings
  let bhEnv = V5.mkBHEnv
              (V5.Server "http://localhost:9200")
              connManager
      indexName = V5.IndexName "katip-elasticsearch-bench"
      mappingName = V5.MappingName "k-e-b-log"
      severity = DebugS
      verbosity = V0
      Just queueSize = mkEsQueueSize 1000
      Just poolSize = mkEsPoolSize 10
      scribeCfg =
        defaultEsScribeCfgV5 { essQueueSize = queueSize
                             , essPoolSize = poolSize
                             , essChunkSize = 500
                             }
      indexFlusher = V5.runBH bhEnv (V5.refreshIndex indexName)
  esScribe <- mkEsScribe
            scribeCfg bhEnv indexName
            mappingName severity verbosity
  let mkLogEnv = registerScribe "es" esScribe defaultScribeSettings =<< initLogEnv "MyApp" "production"
  return (indexFlusher, mkLogEnv)

logMessagesBulk :: IORef Integer -> Int -> IO ()
logMessagesBulk docCounter repeats = do
  (indexFlusher, mkLogEnv) <- mkEsBenchLogEnvBulk
  bracket mkLogEnv (finalizer indexFlusher)
    $ \ le -> forM_ [1..repeats] $ \i -> runKatipT le $ do
      liftIO $ atomicModifyIORef' docCounter (\x -> (x+1, ()))
      logMsg "ns" InfoS ("This goes to elasticsearch: "
                         <> (logStr $ T.pack $ show i))
  where
    finalizer :: IO Reply -> LogEnv -> IO LogEnv
    finalizer indexFlusher le = do
      -- putStrLn "Finalizer was called"
      _ <- indexFlusher
      -- threadDelay 1000000
      closeScribes le

mkEsBenchLogEnvBulk :: IO (IO Reply, IO LogEnv)
mkEsBenchLogEnvBulk = do
  connManager <- HTTP.newManager HTTP.defaultManagerSettings
  let bhEnv = V5.mkBHEnv
              (V5.Server "http://localhost:9200")
              connManager
      indexName = V5.IndexName "katip-elasticsearch-bench"
      mappingName = V5.MappingName "k-e-b-log"
      severity = DebugS
      verbosity = V0
      Just queueSize = mkEsQueueSize 1000
      Just poolSize = mkEsPoolSize 10
      scribeCfg =
        defaultEsScribeCfgV5 { essQueueSize = queueSize
                             , essPoolSize = poolSize
                             , essChunkSize = 500
                             }
      indexFlusher = V5.runBH bhEnv (V5.refreshIndex indexName)
  esScribe <- mkEsBulkScribe
            scribeCfg bhEnv indexName
            mappingName severity verbosity
  let mkLogEnv = registerScribe "es" esScribe defaultScribeSettings =<< initLogEnv "MyApp" "production"
  return (indexFlusher, mkLogEnv)

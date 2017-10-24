{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where


import           Control.Exception
import           Control.Monad (replicateM_)
import           Database.V5.Bloodhound
import           Network.HTTP.Client

import           Katip
import           Katip.Scribes.ElasticSearch


main :: IO ()
main = do
  mgr <- newManager defaultManagerSettings
  let bhe = mkBHEnv
              (Server "http://localhost:9200")
              mgr
      indexName = IndexName "katip-elasticsearch-bulk-example"
      mappingName = MappingName "k-e-b-log"
      severity = DebugS
      verbosity = V0
      
      Just queueSize = mkEsQueueSize 1000
      Just poolSize = mkEsPoolSize 10

  esScribe <- mkEsBulkScribe
    -- Reasonable for production
    defaultEsScribeCfgV5 { essQueueSize = queueSize
                         , essPoolSize = poolSize
                         }
    -- Reasonable for single-node in development
    -- defaultEsScribeCfgV5 { essIndexSettings = IndexSettings (ShardCound 1) (ReplicaCount 0)}
    bhe
    indexName
    mappingName
    DebugS
    V3
  let mkLogEnv = registerScribe "es" esScribe defaultScribeSettings
                 =<< initLogEnv "MyApp" "production"
  bracket mkLogEnv closeScribes $ \ le -> replicateM_ 100000 $ runKatipT le $ do
    logMsg "ns" InfoS "This goes to elasticsearch"

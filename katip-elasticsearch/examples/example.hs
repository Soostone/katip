{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where


import           Control.Exception
import           Database.V5.Bloodhound
import           Network.HTTP.Client

import           Katip
import           Katip.Scribes.ElasticSearch


main :: IO ()
main = do
  mgr <- newManager defaultManagerSettings
  let bhe = mkBHEnv (Server "localhost") mgr
  esScribe <- mkEsScribe
    -- Reasonable for production
    defaultEsScribeCfgV5
    -- Reasonable for single-node in development
    -- defaultEsScribeCfgV5 { essIndexSettings = IndexSettings (ShardCound 1) (ReplicaCount 0)}
    bhe
    (IndexName "all-indices-prefixed-with")
    (MappingName "application-logs")
    DebugS
    V3
  let mkLogEnv = registerScribe "es" esScribe defaultScribeSettings =<< initLogEnv "MyApp" "production"
  bracket mkLogEnv closeScribes $ \le -> runKatipT le $ do
    logMsg "ns" InfoS "This goes to elasticsearch"

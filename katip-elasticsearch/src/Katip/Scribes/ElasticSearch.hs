{-# LANGUAGE CPP #-}
-- | Includes a scribe that can be used to log structured, JSON log
-- messages to ElasticSearch. These logs can be explored easily using
-- <https://www.elastic.co/products/kibana kibana> or your tool of
-- choice. Supports ElasticSearch servers with version 1.x or 5.x by
-- way of different configs.
--
-- Example of configuring for ES5:
--
-- @
--
-- import           Control.Exception
-- import           Database.V5.Bloodhound
-- import           Network.HTTP.Client
-- import           Katip
-- import           Katip.Scribes.ElasticSearch
--
-- main :: IO ()
-- main = do
--   mgr <- newManager defaultManagerSettings
--   let bhe = mkBHEnv (Server "http://localhost:9200") mgr
--   esScribe <- mkEsScribe
--     -- Reasonable for production
--     defaultEsScribeCfgV5
--     -- Reasonable for single-node in development
--     -- defaultEsScribeCfgV5 { essIndexSettings = IndexSettings (ShardCound 1) (ReplicaCount 0)} :: EsScribeCfg ESV5
--     bhe
--       (IndexName "all-indices-prefixed-with")
--       (MappingName "application-logs")
--       DebugS
--       V3
--   let mkLogEnv = registerScribe "es" esScribe defaultScribeSettings =<< initLogEnv \"MyApp\" "production"
--   bracket mkLogEnv closeScribes $ \\le -> runKatipT le $ do
--     logMsg "ns" InfoS "This goes to elasticsearch"
--
-- @
--
-- __Important Note on Index Settings__
--
-- 'defaultEsScribeCfg' inherits a set of default index settings from
-- the @bloodhound@ package. These settings at this time of writing
-- set the indices up to have 3 shards and 2 replicas. This is an
-- arguably reasonable default setting for production but may cause
-- problems for development. In development, your cluster may be
-- configured to seek a write quorum greater than 1. If you're running
-- ElasticSearch on a single node, this could cause your writes to
-- wait for a bit and then fail due to a lack of quorum. __For development, we recommend setting your replica count to 0 or modifying your write quorum settings__. For production, we recommend reading the
-- <https://www.elastic.co/guide/en/elasticsearch/guide/current/scale.html ElasticSearch Scaling Guide> and choosing the appropriate settings,
-- keeping in mind that you can chage replica counts on a live index
-- but that changing shard counts requires recreating the index.
module Katip.Scribes.ElasticSearch
    (-- * Building a scribe
      mkEsScribe
    -- * Scribe configuration
    , EsScribeSetupError(..)
    , EsQueueSize
    , mkEsQueueSize
    , EsPoolSize
    , mkEsPoolSize
    , IndexShardingPolicy(..)
    , IndexNameSegment(..)
    -- ** EsScribeCfg and fields
    , EsScribeCfg
    , essRetryPolicy
    , essQueueSize
    , essPoolSize
    , essAnnotateTypes
    , essIndexSettings
    , essIndexSharding
    , defaultEsScribeCfg
#if !MIN_VERSION_bloodhound(0,17,0)
    , defaultEsScribeCfgV1
#endif
    , defaultEsScribeCfgV5
    -- ** Version-Proxied APIS
    -- $versionproxies
    , defaultEsScribeCfg'
#if !MIN_VERSION_bloodhound(0,17,0)
    , ESV1
#endif
    , ESV5
    -- * Utilities
    , mkDocId
    , module Katip.Scribes.ElasticSearch.Annotations
    ) where


-------------------------------------------------------------------------------
import           Katip.Scribes.ElasticSearch.Annotations
import           Katip.Scribes.ElasticSearch.Internal
-------------------------------------------------------------------------------


{- $versionproxies

  You may need these these functions and types if type inference
  fails. For instance, you may need to hint to the compiler that a
  config is @:: EsScribeCfg ESV5@, for instance.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
    ( main
    ) where

-------------------------------------------------------------------------------
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Criterion.Main
import           Data.Aeson
import qualified Data.HashMap.Strict                  as HM
import           Data.Monoid
import           Data.Proxy                           (Proxy (..))
import           Data.RNG
import qualified Data.Text                            as T
import           Data.Time.Clock
import           Database.V1.Bloodhound.Types
import qualified Database.V5.Bloodhound.Types         as V5
import qualified Network.HTTP.Client                  as HTTP
import           Numeric

import           Katip
-------------------------------------------------------------------------------
import           Katip.Scribes.ElasticSearch
import           Katip.Scribes.ElasticSearch.Internal (ESV1)
-------------------------------------------------------------------------------

main :: IO ()
main = do
  mkLogEnv <- mkEsBenchLogEnv
  rng <- mkRNG
  defaultMain
    [
    --   mkDocIdBenchmark rng
    -- , deannotateValueBenchmark
    esLoggingBenchmark mkLogEnv
    ]

-------------------------------------------------------------------------------
mkDocIdBenchmark :: RNG -> Benchmark
mkDocIdBenchmark rng = bgroup "mkDocId"
  [
    bench "mkDocId (randomIO)" $ nfIO (mkDocId (Proxy :: Proxy ESV1))
  , bench "mkDocId' (shared )" $ nfIO $ mkDocId' rng
  ]


-------------------------------------------------------------------------------
deannotateValueBenchmark :: Benchmark
deannotateValueBenchmark = bgroup "deannotateValue"
 [
   bench "deannotateValue" $ nf deannotateValue annotatedValue
 ]


-------------------------------------------------------------------------------
annotatedValue :: Value
annotatedValue = Object $ HM.fromList [ ("a::string", String "whatever")
                                      , ("b::double", Number 4.5)
                                      , ("c::long", Number 4)
                                      , ("d::boolean", Bool True)
                                      , ("e::null", Null)
                                      ]

-------------------------------------------------------------------------------
mkDocId' :: RNG -> IO DocId
mkDocId' rng = do
    is <- withRNG rng $ \gen -> replicateM len $ mk gen
    return . DocId . T.pack . concatMap (`showHex` "") $ is
  where
    len = 32
    mk :: GenIO -> IO Int
    mk = uniformR (0,15)

deriving instance NFData DocId

esLoggingBenchmark :: IO LogEnv -> Benchmark
esLoggingBenchmark mkLogEnv = bgroup "ES logging"
 [
   bench "log 10 messages" $ nfIO (logMessages mkLogEnv 10)
 , bench "log 100 messages" $ nfIO (logMessages mkLogEnv 100)
 , bench "log 1000 Messages" $ nfIO (logMessages mkLogEnv 1000)
 ]

logMessages :: IO LogEnv -> Int -> IO ()
logMessages mkLogEnv repeats = do
  bracket mkLogEnv closeScribes
    $ \ le -> forM_ [1..repeats] $ \i -> runKatipT le $ do
      logMsg "ns" InfoS ("This goes to elasticsearch: " <> (logStr $ T.pack $ show i))

mkEsBenchLogEnv :: IO (IO LogEnv)
mkEsBenchLogEnv = do
  connManager <- HTTP.newManager HTTP.defaultManagerSettings
  let bhEnv = V5.mkBHEnv
              (V5.Server "http://localhost:9200")
              connManager
      indexName = V5.IndexName "katip-elasticsearch-bench"
      mappingName = V5.MappingName "k-e-b-log"
      severity = DebugS
      verbosity = V0
  esScribe <- mkEsScribe
            defaultEsScribeCfgV5 bhEnv indexName
            mappingName severity verbosity
  let mkLogEnv = registerScribe "es" esScribe defaultScribeSettings =<< initLogEnv "MyApp" "production"
  return mkLogEnv

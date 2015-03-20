{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
    ( main
    ) where

-------------------------------------------------------------------------------
import           Control.DeepSeq
import           Control.Monad
import           Criterion.Main
import           Data.RNG
import qualified Data.Text                   as T
import           Database.Bloodhound.Types
import           Numeric
-------------------------------------------------------------------------------
import           Katip.Scribes.ElasticSearch
-------------------------------------------------------------------------------

main :: IO ()
main = do
    rng <- mkRNG
    defaultMain [mkDocIdBenchmark rng]

-------------------------------------------------------------------------------
mkDocIdBenchmark :: RNG -> Benchmark
mkDocIdBenchmark rng = bgroup "mkDocId"
  [ bench "mkDocId (randomIO)" $ nfIO mkDocId
  , bench "mkDocId' (shared )" $ nfIO $ mkDocId' rng
  ]


mkDocId' :: RNG -> IO DocId
mkDocId' rng = do
    is <- withRNG rng $ \gen -> replicateM len $ mk gen
    return . DocId . T.pack . concatMap (`showHex` "") $ is
  where
    len = 32
    mk :: GenIO -> IO Int
    mk = uniformR (0,15)

deriving instance NFData DocId

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
import           Data.Aeson
import qualified Data.HashMap.Strict                     as HM
import           Data.RNG
import qualified Data.Text                               as T
import           Database.Bloodhound.Types
import           Numeric
-------------------------------------------------------------------------------
import           Katip.Scribes.ElasticSearch
import           Katip.Scribes.ElasticSearch.Annotations
-------------------------------------------------------------------------------

main :: IO ()
main = do
    rng <- mkRNG
    defaultMain
      [
        mkDocIdBenchmark rng
      , deannotateValueBenchmark
      ]

-------------------------------------------------------------------------------
mkDocIdBenchmark :: RNG -> Benchmark
mkDocIdBenchmark rng = bgroup "mkDocId"
  [
    bench "mkDocId (randomIO)" $ nfIO mkDocId
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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
module Katip.Scribes.ElasticSearch
    (-- * Building a scribe
      mkEsScribe
    -- * Scribe configuration
    , EsQueueSize
    , mkEsQueueSize
    , EsPoolSize
    , mkEsPoolSize
    , EsScribeCfg
    , essManagerSettings
    , essRetryPolicy
    , essQueueSize
    , essPoolSize
    , defaultEsScribeCfg
    -- * Utilities
    , mkDocId
    ) where


-------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.STM
import           Control.Concurrent.STM.TBMQueue
import           Control.Exception.Enclosed
import           Control.Monad
import           Control.Retry
import           Data.Aeson
import qualified Data.Text.Encoding              as T
import           Data.Typeable
import           Data.UUID
import           Database.Bloodhound
import           Network.HTTP.Client
import           System.Random
-------------------------------------------------------------------------------
import           Katip.Core
-------------------------------------------------------------------------------


data EsScribeCfg = EsScribeCfg {
      essManagerSettings :: ManagerSettings
    -- ^ Connection manager settings
    , essRetryPolicy     :: RetryPolicy
    -- ^ Retry policy when there are errors sending logs to the server
    , essQueueSize       :: EsQueueSize
    -- ^ Maximum size of the bounded log queue
    , essPoolSize        :: EsPoolSize
    -- ^ Worker pool size limit for sending data to the
    } deriving (Typeable)


-- | Reasonable defaults for a config:
--
--     * defaultManagerSettings
--
--     * exponential backoff with 25ms base delay up to 5 retries
--
--     * Queue size of 1000
--
--     * Pool size of 2
defaultEsScribeCfg :: EsScribeCfg
defaultEsScribeCfg = EsScribeCfg {
      essManagerSettings = defaultManagerSettings
    , essRetryPolicy = exponentialBackoff 25 <> limitRetries 5
    , essQueueSize = EsQueueSize 1000
    , essPoolSize = EsPoolSize 2
    }


-------------------------------------------------------------------------------
mkEsScribe
    :: EsScribeCfg
    -> Server
    -> IndexName
    -> MappingName
    -> Severity
    -> Verbosity
    -> IO (Scribe, IO ())
    -- ^ Returns a finalizer that will gracefully flush all remaining logs before shutting down workers
mkEsScribe cfg@EsScribeCfg {..} server ix mapping sev verb = do
  q <- newTBMQueueIO $ unEsQueueSize essQueueSize
  withManager essManagerSettings $ \mgr -> do
    let env = BHEnv { bhServer = server
                    , bhManager = mgr
                    }
    endSig <- newEmptyMVar

    workers <- replicateM (unEsPoolSize essPoolSize) $ async $
      startWorker cfg env ix mapping q

    _ <- async $ do
      takeMVar endSig
      atomically $ closeTBMQueue q
      -- wait for queue to empty
      atomically $ check =<< isEmptyTBMQueue q
      mapM_ cancel workers
      putMVar endSig ()

    let scribe = Scribe $ \ i ->
          when (_itemSeverity i >= sev) $
          void $ atomically $ tryWriteTBMQueue q (itemJson verb i)
    let finalizer = putMVar endSig () >> takeMVar endSig
    return (scribe, finalizer)


-------------------------------------------------------------------------------
mkDocId :: IO DocId
mkDocId = (DocId . T.decodeUtf8 . toASCIIBytes) `fmap` randomIO


-------------------------------------------------------------------------------
newtype EsQueueSize = EsQueueSize {
       unEsQueueSize :: Int
     } deriving (Show, Eq, Ord)


instance Bounded EsQueueSize where
  minBound = EsQueueSize 1
  maxBound = EsQueueSize maxBound


mkEsQueueSize :: Int -> Maybe EsQueueSize
mkEsQueueSize = mkNonZero EsQueueSize


-------------------------------------------------------------------------------
newtype EsPoolSize = EsPoolSize {
      unEsPoolSize :: Int
    } deriving (Show, Eq, Ord)


instance Bounded EsPoolSize where
  minBound = EsPoolSize 1
  maxBound = EsPoolSize maxBound


mkEsPoolSize :: Int -> Maybe EsPoolSize
mkEsPoolSize = mkNonZero EsPoolSize


-------------------------------------------------------------------------------
mkNonZero :: (Int -> a) -> Int -> Maybe a
mkNonZero ctor n
  | n > 0     = Just $ ctor n
  | otherwise = Nothing


-------------------------------------------------------------------------------
startWorker
    :: EsScribeCfg
    -> BHEnv
    -> IndexName
    -> MappingName
    -> TBMQueue Value
    -> IO ()
startWorker EsScribeCfg {..} env ix mapping q = forever $ do
    v <- atomically $ readTBMQueue q
    sendLog v `catchAny` eat
  where
    sendLog v = void $ recoverAll essRetryPolicy $ do
      did <- mkDocId
      runBH env $ indexDocument ix mapping v did
    eat _ = return ()

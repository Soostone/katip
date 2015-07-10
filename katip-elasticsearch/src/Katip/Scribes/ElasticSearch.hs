{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    , essAnnotateTypes
    , defaultEsScribeCfg
    -- * Utilities
    , mkDocId
    , module Katip.Scribes.ElasticSearch.Annotations
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TBMQueue
import           Control.Exception.Base
import           Control.Exception.Enclosed
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.STM
import           Control.Retry                           (RetryPolicy,
                                                          exponentialBackoff,
                                                          limitRetries,
                                                          recovering)
import           Data.Aeson
import           Data.Monoid                             ((<>))
import qualified Data.Text.Encoding                      as T
import           Data.Typeable
import           Data.UUID
import           Database.Bloodhound
import           Network.HTTP.Client
import           System.Random
-------------------------------------------------------------------------------
import           Katip.Core
import           Katip.Scribes.ElasticSearch.Annotations
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
    , essAnnotateTypes   :: Bool
    -- ^ Different payload items coexist in the "data" attribute in
    -- ES. It is possible for different payloads to have different
    -- types for the same key, e.g. an "id" key that is sometimes a
    -- number and sometimes a string. If you're having ES do dynamic
    -- mapping, the first log item will set the type and any that
    -- don't conform will be *discarded*. If you set this to True,
    -- keys will recursively be appended with their ES core
    -- type. e.g. "id" would become "id::long" and "id::string"
    -- automatically, so they won't conflict. When this library
    -- exposes a querying API, we will try to make deserialization and
    -- querying transparently remove the type annotations if this is
    -- enabled.
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
--
--     * Annotate types set to False
defaultEsScribeCfg :: EsScribeCfg
defaultEsScribeCfg = EsScribeCfg {
      essManagerSettings = defaultManagerSettings
    , essRetryPolicy     = exponentialBackoff 25 <> limitRetries 5
    , essQueueSize       = EsQueueSize 1000
    , essPoolSize        = EsPoolSize 2
    , essAnnotateTypes   = False
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
  mgr <- newManager essManagerSettings
  let env = BHEnv { bhServer = server
                  , bhManager = mgr
                  }
  endSig <- newEmptyMVar

  workers <- replicateM (unEsPoolSize essPoolSize) $ async $
    startWorker cfg env ix mapping q

  _ <- async $ do
    takeMVar endSig
    atomically $ closeTBMQueue q
    mapM_ wait workers
    closeManager mgr
    putMVar endSig ()

  let scribe = Scribe $ \ i ->
        when (_itemSeverity i >= sev) $
          void $ atomically $ tryWriteTBMQueue q (itemJson' verb i)
  let finalizer = putMVar endSig () >> takeMVar endSig
  return (scribe, finalizer)
  where
    itemJson' v i
      | essAnnotateTypes = itemJson v (TypeAnnotated <$> i)
      | otherwise        = itemJson v i


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
startWorker EsScribeCfg {..} env ix mapping q = go -- manual recursion here. is this a space leak
  where
    go = do
      v <- atomically $ readTBMQueue q
      case v of
        Just v' -> do
          sendLog v' `catchAny` eat
          go
        Nothing -> return ()
    sendLog v = void $ recovering essRetryPolicy [handler] $ do
      did <- mkDocId
      runBH env $ indexDocument ix mapping defaultIndexDocumentSettings v did
    eat _ = return ()
    handler _ = Handler $ \e ->
      case fromException e of
        Just (_ :: AsyncException) -> return False
        _ -> return True

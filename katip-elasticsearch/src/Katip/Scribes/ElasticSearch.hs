{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Katip.Scribes.ElasticSearch
    (-- * Building a scribe
      mkEsScribe
    -- * Scribe configuration
    , EsScribeSetupError(..)
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
    , essIndexSettings
    , essIndexSharding
    , IndexShardingPolicy(..)
    , IndexNameSegment(..)
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
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Retry                           (RetryPolicy,
                                                          exponentialBackoff,
                                                          limitRetries,
                                                          recovering)
import           Data.Aeson
import           Data.Monoid                             ((<>))
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import qualified Data.Text.Encoding                      as T
import           Data.Time
import           Data.Typeable
import           Data.UUID
import           Database.Bloodhound
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
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
    , essIndexSettings   :: IndexSettings
    , essIndexSharding   :: IndexShardingPolicy
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
    , essIndexSettings   = defaultIndexSettings
    , essIndexSharding   = NoIndexSharding
    }


-------------------------------------------------------------------------------
-- | How should katip store your log data?
--
-- * NoIndexSharding will store all logs in one index name. This is
-- the simplest option but is not advised in production. In practice,
-- the index will grow very large and will get slower to
-- search. Deleting records based on some sort of retention period is
-- also extremely slow.
--
-- * MonthlyIndexSharding, DailyIndexSharding, HourlyIndexSharding,
-- EveryMinuteIndexSharding will generate indexes based on the time of
-- the log. Index name is treated as a prefix. So if your index name
-- is @foo@ and DailySharding is used, logs will be stored in
-- @foo-2016-02-25@, @foo-2016-02-26@ and so on. Index templating will
-- be used to set up mappings automatically. Deletes based on date are
-- very fast and queries can be restricted to date ranges for better
-- performance. Queries against all dates should use @foo-*@ as an
-- index name. Note that index aliasing's glob feature is not suitable
-- for these date ranges as it matches index names as they are
-- declared, so new dates will be excluded. DailyIndexSharding is a
-- reasonable choice. Changing index sharding strategies is a *bad
-- idea*.
--
-- * CustomSharding: supply your own function that decomposes an item
-- into its index name heirarchy which will be appended to the index
-- name. So for instance if your function return ["arbitrary",
-- "prefix"], the index will be @foo-arbitrary-prefix@ and the index
-- template will be set to match @foo-*@
data IndexShardingPolicy = NoIndexSharding
                         | MonthlyIndexSharding
                         | DailyIndexSharding
                         | HourlyIndexSharding
                         | EveryMinuteIndexSharding
                         | CustomIndexSharding (forall a. Item a -> [IndexNameSegment])


instance Show IndexShardingPolicy where
 show NoIndexSharding          = "NoIndexSharding"
 show MonthlyIndexSharding     = "MonthlyIndexSharding"
 show DailyIndexSharding       = "DailyIndexSharding"
 show HourlyIndexSharding      = "HourlyIndexSharding"
 show EveryMinuteIndexSharding = "EveryMinuteIndexSharding"
 show (CustomIndexSharding _)  = "CustomIndexSharding λ"


-------------------------------------------------------------------------------
newtype IndexNameSegment = IndexNameSegment {
      indexNameSegment :: Text
    } deriving (Show, Eq, Ord)


-------------------------------------------------------------------------------
shardPolicySegs :: IndexShardingPolicy -> Item a -> [IndexNameSegment]
shardPolicySegs NoIndexSharding _ = []
shardPolicySegs MonthlyIndexSharding Item {..} = [sis y, sis m]
  where
    (y, m, _) = toGregorian (utctDay _itemTime)
shardPolicySegs DailyIndexSharding Item {..} = [sis y, sis m, sis d]
  where
    (y, m, d) = toGregorian (utctDay _itemTime)
shardPolicySegs HourlyIndexSharding Item {..} = [sis y, sis m, sis d, sis h]
  where
    (y, m, d) = toGregorian (utctDay _itemTime)
    (h, _) = splitTime (utctDayTime _itemTime)
shardPolicySegs EveryMinuteIndexSharding Item {..} = [sis y, sis m, sis d, sis h, sis mn]
  where
    (y, m, d) = toGregorian (utctDay _itemTime)
    (h, mn) = splitTime (utctDayTime _itemTime)
shardPolicySegs (CustomIndexSharding f) i  = f i


-------------------------------------------------------------------------------
chooseIxn :: IndexName -> IndexShardingPolicy -> Item a -> IndexName
chooseIxn (IndexName ixn) p i =
  IndexName (T.intercalate "-" (ixn:segs))
  where
    segs = indexNameSegment <$> shardPolicySegs p i


-------------------------------------------------------------------------------
sis :: Show a => a -> IndexNameSegment
sis = IndexNameSegment . T.pack . show


-------------------------------------------------------------------------------
splitTime :: DiffTime -> (Int, Int)
splitTime t = asMins `divMod` 60
  where
    asMins = floor t `div` 60


-------------------------------------------------------------------------------
data EsScribeSetupError = CouldNotCreateIndex !Reply
                        | CouldNotCreateMapping !Reply deriving (Typeable, Show)


instance Exception EsScribeSetupError

-------------------------------------------------------------------------------
mkEsScribe
    :: EsScribeCfg
    -> Server
    -> IndexName
    -- ^ Treated as a prefix if index sharding is enabled
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

  runBH env $ do
    chk <- indexExists ix
    -- note that this doesn't update settings. That's not available
    -- through the Bloodhound API yet
    unless chk $ void $ do
      r1 <- createIndex essIndexSettings ix
      unless (statusIsSuccessful (responseStatus r1)) $
        liftIO $ throwIO (CouldNotCreateIndex r1)
      --TODO: throw on error
      r2 <- if shardingEnabled
              then putTemplate tpl tplName
              else putMapping ix mapping (baseMapping mapping)
      unless (statusIsSuccessful (responseStatus r2)) $
        liftIO $ throwIO (CouldNotCreateMapping r2)

  workers <- replicateM (unEsPoolSize essPoolSize) $ async $
    startWorker cfg env mapping q

  _ <- async $ do
    takeMVar endSig
    atomically $ closeTBMQueue q
    mapM_ waitCatch workers
    putMVar endSig ()

  let scribe = Scribe $ \ i ->
        when (_itemSeverity i >= sev) $
          void $ atomically $ tryWriteTBMQueue q (chooseIxn ix essIndexSharding i, itemJson' i)
  let finalizer = putMVar endSig () >> takeMVar endSig
  return (scribe, finalizer)
  where
    tplName = TemplateName ixn
    shardingEnabled = case essIndexSharding of
      NoIndexSharding -> False
      _               -> True
    tpl = IndexTemplate (TemplatePattern (ixn <> "-*")) (Just essIndexSettings) [toJSON (baseMapping mapping)]
    IndexName ixn = ix
    itemJson' i
      | essAnnotateTypes = itemJson verb (TypeAnnotated <$> i)
      | otherwise        = itemJson verb i


-------------------------------------------------------------------------------
baseMapping :: MappingName -> Value
baseMapping (MappingName mn) =
  object [ mn .= object ["properties" .= object pairs] ]
  where pairs = [ str "thread"
                , str "sev"
                , str "pid"
                , str "ns"
                , str "msg"
                , "loc" .= locType
                , str "host"
                , str "env"
                , "at" .= dateType
                , str "app"
                ]
        str k = k .= object ["type" .= String "string"]
        locType = object ["properties" .= object locPairs]
        locPairs = [ str "loc_pkg"
                   , str "loc_mod"
                   , str "loc_ln"
                   , str "loc_fn"
                   , str "loc_col"
                   ]
        dateType = object [ "format" .= esDateFormat
                          , "type" .= String "date"
                          ]


-------------------------------------------------------------------------------
-- | Handle both old-style aeson and picosecond-level precision
esDateFormat :: Text
esDateFormat = "yyyy-MM-dd'T'HH:mm:ss.SSSZ||yyyy-MM-dd'T'HH:mm:ss.SSSSSSSSSSSSZ"


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
    -> MappingName
    -> TBMQueue (IndexName, Value)
    -> IO ()
startWorker EsScribeCfg {..} env mapping q = go
  where
    go = do
      popped <- atomically $ readTBMQueue q
      case popped of
        Just (ixn, v) -> do
          sendLog ixn v `catchAny` eat
          go
        Nothing -> return ()
    sendLog :: IndexName -> Value -> IO ()
    sendLog ixn v = void $ recovering essRetryPolicy [handler] $ const $ do
      did <- mkDocId
      runBH env $ indexDocument ixn mapping defaultIndexDocumentSettings v did
    eat _ = return ()
    handler _ = Handler $ \e ->
      case fromException e of
        Just (_ :: AsyncException) -> return False
        _ -> return True

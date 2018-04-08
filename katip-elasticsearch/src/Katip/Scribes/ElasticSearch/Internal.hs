{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- | This is an internal module. No guarantees are made in this module
-- about API stability.
module Katip.Scribes.ElasticSearch.Internal where


import           Control.Applicative                     as A
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TBMQueue
import qualified Control.Concurrent.Chan.Unagi.Bounded   as UNB
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
import           Data.ByteString.Lazy                    (ByteString)
import           Data.Monoid                             ((<>))
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import qualified Data.Text.Encoding                      as T
import           Data.Time
import           Data.Time.Calendar.WeekDate
import           Data.Typeable                           as Typeable
import           Data.UUID
import qualified Data.UUID.V4                            as UUID4
import qualified Data.Vector                             as V
import qualified Database.V1.Bloodhound                  as V1
import qualified Database.V5.Bloodhound                  as V5
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           System.Random                           (randomRIO)
-- import           System.Timeout                          (timeout)
import           Text.Printf                             (printf)

import           Katip.Core
import           Katip.Scribes.ElasticSearch.Annotations


-- | EsScribeCfg now carries a type variable for the version of
-- ElasticSearch it targets, either 'ESV1' or 'ESV5'. You can use
-- 'defaultEsScribeCfgV1' and 'defaultESScribeCfgV5' for a good
-- starting point depending on the ES version you have.
data EsScribeCfg v = EsScribeCfg {
      essRetryPolicy   :: RetryPolicy
    -- ^ Retry policy when there are errors sending logs to the server
    , essQueueSize     :: EsQueueSize
    -- ^ Maximum size of the bounded log queue
    , essPoolSize      :: EsPoolSize
    -- ^ Worker pool size limit for sending data to the
    , essAnnotateTypes :: Bool
    -- ^ Different payload items coexist in the "data" attribute in
    -- ES. It is possible for different payloads to have different
    -- types for the same key, e.g. an "id" key that is sometimes a
    -- number and sometimes a string. If you're having ES do dynamic
    -- mapping, the first log item will set the type and any that
    -- don't conform will be *discarded*. If you set this to True,
    -- keys will recursively be appended with their ES core
    -- type. e.g. "id" would become "id::l" and "id::s"
    -- automatically, so they won't conflict. When this library
    -- exposes a querying API, we will try to make deserialization and
    -- querying transparently remove the type annotations if this is
    -- enabled.
    , essIndexSettings :: IndexSettings v
    -- ^ This will be the IndexSettings type from the appropriate
    -- bloodhound module, either @Database.V1.Bloodhound@ or
    -- @Database.V5.Bloodhound@
    , essIndexSharding :: IndexShardingPolicy
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
--
--     * DailyIndexSharding
defaultEsScribeCfg' :: ESVersion v => proxy v -> EsScribeCfg v
defaultEsScribeCfg' prx = EsScribeCfg {
      essRetryPolicy     = exponentialBackoff 25 <> limitRetries 5
    , essQueueSize       = EsQueueSize 1000
    , essPoolSize        = EsPoolSize 2
    , essAnnotateTypes   = False
    , essIndexSettings   = defaultIndexSettings prx
    , essIndexSharding   = DailyIndexSharding
    }


-- | Alias of 'defaultEsScribeCfgV1' to minimize API
-- breakage. Previous versions of katip-elasticsearch only supported
-- ES version 1.
defaultEsScribeCfg :: EsScribeCfg ESV1
defaultEsScribeCfg = defaultEsScribeCfgV1


-- | EsScribeCfg that will use ElasticSearch V1
defaultEsScribeCfgV1 :: EsScribeCfg ESV1
defaultEsScribeCfgV1 = defaultEsScribeCfg' (Typeable.Proxy :: Typeable.Proxy ESV1)


-- | EsScribeCfg that will use ElasticSearch V5
defaultEsScribeCfgV5 :: EsScribeCfg ESV5
defaultEsScribeCfgV5 = defaultEsScribeCfg' (Typeable.Proxy :: Typeable.Proxy ESV5)


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
-- @foo-2016-2-25@, @foo-2016-2-26@ and so on. Index templating will
-- be used to set up mappings automatically. Deletes based on date are
-- very fast and queries can be restricted to date ranges for better
-- performance. Queries against all dates should use @foo-*@ as an
-- index name. Note that index aliasing's glob feature is not suitable
-- for these date ranges as it matches index names as they are
-- declared, so new dates will be excluded. DailyIndexSharding is a
-- reasonable choice. Changing index sharding strategies is not
-- advisable.
--
-- * CustomSharding: supply your own function that decomposes an item
-- into its index name hierarchy which will be appended to the index
-- name. So for instance if your function return ["arbitrary",
-- "prefix"], the index will be @foo-arbitrary-prefix@ and the index
-- template will be set to match @foo-*@. In general, you want to use
-- segments of increasing granularity (like year, month, day for
-- dates). This makes it easier to address groups of indexes
-- (e.g. @foo-2016-*@).
data IndexShardingPolicy = NoIndexSharding
                         | MonthlyIndexSharding
                         | WeeklyIndexSharding
                         -- ^ A special case of daily which shards to sunday
                         | DailyIndexSharding
                         | HourlyIndexSharding
                         | EveryMinuteIndexSharding
                         | CustomIndexSharding (forall a. Item a -> [IndexNameSegment])


instance Show IndexShardingPolicy where
 show NoIndexSharding          = "NoIndexSharding"
 show MonthlyIndexSharding     = "MonthlyIndexSharding"
 show WeeklyIndexSharding      = "WeeklyIndexSharding"
 show DailyIndexSharding       = "DailyIndexSharding"
 show HourlyIndexSharding      = "HourlyIndexSharding"
 show EveryMinuteIndexSharding = "EveryMinuteIndexSharding"
 show (CustomIndexSharding _)  = "CustomIndexSharding λ"


newtype IndexNameSegment = IndexNameSegment {
      indexNameSegment :: Text
    } deriving (Show, Eq, Ord)


shardPolicySegs :: IndexShardingPolicy -> Item a -> [IndexNameSegment]
shardPolicySegs NoIndexSharding _ = []
shardPolicySegs MonthlyIndexSharding Item {..} = [sis y, sis m]
  where
    (y, m, _) = toGregorian (utctDay _itemTime)
shardPolicySegs WeeklyIndexSharding Item {..} = [sis y, sis m, sis d]
  where
    (y, m, d) = toGregorian (roundToSunday (utctDay _itemTime))
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


-- | If the given day is sunday, returns the input, otherwise returns
-- the previous sunday
roundToSunday :: Day -> Day
roundToSunday d
    | dow == 7  = d
    | w > 1     = fromWeekDate y (w - 1) 7
    | otherwise = fromWeekDate (y - 1) 53 7
  where
    (y, w, dow) = toWeekDate d


chooseIxn :: ESVersion v
          => proxy v
          -> IndexName v
          -> IndexShardingPolicy
          -> Item a
          -> IndexName v
chooseIxn prx ixn p i =
  toIndexName prx (T.intercalate "-" ((fromIndexName prx ixn):segs))
  where
    segs = indexNameSegment A.<$> shardPolicySegs p i


sis :: Integral a => a -> IndexNameSegment
sis = IndexNameSegment . T.pack . fmt
  where
    fmt = printf "%02d" . toInteger


splitTime :: DiffTime -> (Int, Int)
splitTime t = asMins `divMod` 60
  where
    asMins = floor t `div` 60


data EsScribeSetupError =
    CouldNotCreateIndex !(Response ByteString)
  | CouldNotCreateMapping !(Response ByteString)
  deriving (Typeable, Show)


instance Exception EsScribeSetupError


-- | The Any field tagged with a @v@ corresponds to the type of the
-- same name in the corresponding @bloodhound@ module. For instance,
-- if you are configuring for ElasticSearch version 1, import
-- @Database.V1.Bloodhound@ and @BHEnv v@ will refer to @BHEnv@ from
-- that module, @IndexName v@ will repsond to @IndexName@ from that
-- module, etc.
mkEsScribe
    :: forall v. ( ESVersion v
                 , MonadIO (BH v IO)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 800
                 , Functor (BH v IO)
#endif
                 )
    => EsScribeCfg v
    -> BHEnv v
    -> IndexName v
    -- ^ Treated as a prefix if index sharding is enabled
    -> MappingName v
    -> Severity
    -> Verbosity
    -> IO Scribe
mkEsScribe cfg@EsScribeCfg {..} env ix mapping sev verb = do
  q <- newTBMQueueIO $ unEsQueueSize essQueueSize
  endSig <- newEmptyMVar

  runBH prx env $ do
    chk <- indexExists prx ix
    -- note that this doesn't update settings. That's not available
    -- through the Bloodhound API yet
    unless chk $ void $ do
      r1 <- createIndex prx essIndexSettings ix
      unless (statusIsSuccessful (responseStatus r1)) $
        liftIO $ throwIO (CouldNotCreateIndex r1)
      r2 <- if shardingEnabled
              then putTemplate prx tpl tplName
              else putMapping prx ix mapping base
      unless (statusIsSuccessful (responseStatus r2)) $
        liftIO $ throwIO (CouldNotCreateMapping r2)

  workers <- replicateM (unEsPoolSize essPoolSize) $ async $
    startWorker cfg env mapping q

  _ <- async $ do
    takeMVar endSig
    atomically $ closeTBMQueue q
    mapM_ waitCatch workers
    putMVar endSig ()

  let finalizer = putMVar endSig () >> takeMVar endSig
  return (Scribe (logger q) finalizer)
  where
    logger :: forall a. LogItem a => TBMQueue (IndexName v, Value) -> Item a -> IO ()
    logger q i = when (_itemSeverity i >= sev) $
      void $ atomically $ tryWriteTBMQueue q (chooseIxn prx ix essIndexSharding i, itemJson' i)
    prx :: Typeable.Proxy v
    prx = Typeable.Proxy
    tplName = toTemplateName prx ixn
    shardingEnabled = case essIndexSharding of
      NoIndexSharding -> False
      _               -> True
    tpl = toIndexTemplate prx (toTemplatePattern prx (ixn <> "-*")) (Just essIndexSettings) [toJSON base]
    base = baseMapping prx mapping
    ixn = fromIndexName prx ix
    itemJson' :: LogItem a => Item a -> Value
    itemJson' i
      | essAnnotateTypes = itemJson verb (TypeAnnotated <$> i)
      | otherwise        = itemJson verb i


baseMapping :: ESVersion v => proxy v -> MappingName v -> Value
baseMapping prx mn =
  object [ fromMappingName prx mn .= object ["properties" .= object prs] ]
  where prs = [ str "thread"
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


-- | Handle both old-style aeson and picosecond-level precision
esDateFormat :: Text
esDateFormat = "yyyy-MM-dd'T'HH:mm:ssZ||yyyy-MM-dd'T'HH:mm:ss.SSSZ||yyyy-MM-dd'T'HH:mm:ss.SSSSSSSSSSSSZ"


mkDocId' :: IO Text
mkDocId' = (T.decodeUtf8 . toASCIIBytes) `fmap` UUID4.nextRandom

mkDocId :: ESVersion v => proxy v -> IO (DocId v)
mkDocId prx = toDocId prx <$> mkDocId'


newtype EsQueueSize = EsQueueSize {
       unEsQueueSize :: Int
     } deriving (Show, Eq, Ord)


instance Bounded EsQueueSize where
  minBound = EsQueueSize 1
  maxBound = EsQueueSize maxBound


mkEsQueueSize :: Int -> Maybe EsQueueSize
mkEsQueueSize = mkNonZero EsQueueSize


newtype EsPoolSize = EsPoolSize {
      unEsPoolSize :: Int
    } deriving (Show, Eq, Ord)


instance Bounded EsPoolSize where
  minBound = EsPoolSize 1
  maxBound = EsPoolSize maxBound


mkEsPoolSize :: Int -> Maybe EsPoolSize
mkEsPoolSize = mkNonZero EsPoolSize


mkNonZero :: (Int -> a) -> Int -> Maybe a
mkNonZero ctor n
  | n > 0     = Just $ ctor n
  | otherwise = Nothing


startWorker
    :: forall v. (ESVersion v)
    => EsScribeCfg v
    -> BHEnv v
    -> MappingName v
    -> TBMQueue (IndexName v, Value)
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
    prx :: Typeable.Proxy v
    prx = Typeable.Proxy
    sendLog :: IndexName v -> Value -> IO ()
    sendLog ixn v = void $ recovering essRetryPolicy [handler] $ const $ do
      did <- mkDocId prx
      res <- runBH prx env $ indexDocument prx ixn mapping (defaultIndexDocumentSettings prx) v did
      return res
    eat _ = return ()
    handler _ = Handler $ \e ->
      case fromException e of
        Just (_ :: AsyncException) -> return False
        _                          -> return True


-- We are spanning multiple versions of ES which use completely
-- separate types and APIs, but the subset we use is the same for both
-- versions. This will be kept up to date with bloodhound's supported
-- versions and should be minimally visible to the end user.
class ESVersion v where
  -- Types
  type BHEnv v
  type IndexSettings v
  defaultIndexSettings :: proxy v -> IndexSettings v
  type IndexName v
  toIndexName :: proxy v -> Text -> IndexName v
  fromIndexName :: proxy v -> IndexName v -> Text
  type MappingName v
  fromMappingName :: proxy v -> MappingName v -> Text
  type DocId v
  toDocId :: proxy v -> Text -> DocId v
  type BH v :: (* -> *) -> * -> *
  runBH :: proxy v -> BHEnv v -> BH v m a -> m a
  type TemplateName v
  toTemplateName :: proxy v -> Text -> TemplateName v
  type TemplatePattern v
  toTemplatePattern :: proxy v -> Text -> TemplatePattern v
  type IndexTemplate v
  toIndexTemplate :: proxy v -> TemplatePattern v -> Maybe (IndexSettings v) -> [Value] -> IndexTemplate v
  type IndexDocumentSettings v
  defaultIndexDocumentSettings :: proxy v -> IndexDocumentSettings v

  -- Operations
  -- We're deciding on IO here, but it isn't necessary
  indexExists :: proxy v -> IndexName v -> BH v IO Bool
  indexDocument :: ToJSON doc => proxy v -> IndexName v -> MappingName v -> IndexDocumentSettings v -> doc -> DocId v -> BH v IO (Response ByteString)
  createIndex :: proxy v -> IndexSettings v -> IndexName v -> BH v IO (Response ByteString)
  putTemplate :: proxy v -> IndexTemplate v -> TemplateName v -> BH v IO (Response ByteString)
  putMapping :: (ToJSON a) => proxy v -> IndexName v -> MappingName v -> a -> BH v IO (Response ByteString)


data ESV1 = ESV1

instance ESVersion ESV1 where
  type BHEnv ESV1 = V1.BHEnv
  type IndexSettings ESV1 = V1.IndexSettings
  defaultIndexSettings _ = V1.defaultIndexSettings
  type IndexName ESV1 = V1.IndexName
  toIndexName _ = V1.IndexName
  fromIndexName _ (V1.IndexName x) = x
  type MappingName ESV1 = V1.MappingName
  fromMappingName _ (V1.MappingName x) = x
  type DocId ESV1 = V1.DocId
  toDocId _ = V1.DocId
  type BH ESV1 = V1.BH
  runBH _ = V1.runBH
  type TemplateName ESV1 = V1.TemplateName
  toTemplateName _ = V1.TemplateName
  type TemplatePattern ESV1 = V1.TemplatePattern
  toTemplatePattern _ = V1.TemplatePattern
  type IndexTemplate ESV1 = V1.IndexTemplate
  toIndexTemplate _ = V1.IndexTemplate
  type IndexDocumentSettings ESV1 = V1.IndexDocumentSettings
  defaultIndexDocumentSettings _ = V1.defaultIndexDocumentSettings
  indexExists _ = V1.indexExists
  indexDocument _ = V1.indexDocument
  createIndex _ = V1.createIndex
  putTemplate _ = V1.putTemplate
  putMapping _ = V1.putMapping


data ESV5 = ESV5

instance ESVersion ESV5 where
  type BHEnv ESV5 = V5.BHEnv
  type IndexSettings ESV5 = V5.IndexSettings
  defaultIndexSettings _ = V5.defaultIndexSettings
  type IndexName ESV5 = V5.IndexName
  toIndexName _ = V5.IndexName
  fromIndexName _ (V5.IndexName x) = x
  type MappingName ESV5 = V5.MappingName
  fromMappingName _ (V5.MappingName x) = x
  type DocId ESV5 = V5.DocId
  toDocId _ = V5.DocId
  type BH ESV5 = V5.BH
  runBH _ = V5.runBH
  type TemplateName ESV5 = V5.TemplateName
  toTemplateName _ = V5.TemplateName
  type TemplatePattern ESV5 = V5.TemplatePattern
  toTemplatePattern _ = V5.TemplatePattern
  type IndexTemplate ESV5 = V5.IndexTemplate
  toIndexTemplate _ = V5.IndexTemplate
  type IndexDocumentSettings ESV5 = V5.IndexDocumentSettings
  defaultIndexDocumentSettings _ = V5.defaultIndexDocumentSettings
  indexExists _ = V5.indexExists
  indexDocument _ = V5.indexDocument
  createIndex _ = V5.createIndex
  putTemplate _ = V5.putTemplate
  putMapping _ = V5.putMapping


-- | The Any field tagged with a @v@ corresponds to the type of the
-- same name in the corresponding @bloodhound@ module. For instance,
-- if you are configuring for ElasticSearch version 1, import
-- @Database.V1.Bloodhound@ and @BHEnv v@ will refer to @BHEnv@ from
-- that module, @IndexName v@ will repsond to @IndexName@ from that
-- module, etc.
mkEsBulkScribe
    :: forall v. ( ESVersion v
                 , V5.MonadBH (BH v IO)
                 , MonadIO (BH v IO)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 800
                 , Functor (BH v IO)
#endif
                 )
    => EsScribeCfg v
    -> BHEnv v
    -> IndexName v
    -- ^ Treated as a prefix if index sharding is enabled
    -> MappingName v
    -> Severity
    -> Verbosity
    -- -> (Scribe -> IO a)
    -> IO Scribe
mkEsBulkScribe cfg@EsScribeCfg {..} env ix mapping sev verb = do
  (inChan, outChan) <- UNB.newChan $ unEsQueueSize essQueueSize
  endSig <- newEmptyMVar
  workerSig <- newEmptyMVar

  runBH prx env $ do
    chk <- indexExists prx ix
    -- note that this doesn't update settings. That's not available
    -- through the Bloodhound API yet
    unless chk $ void $ do
      r1 <- createIndex prx essIndexSettings ix
      unless (statusIsSuccessful (responseStatus r1)) $
        liftIO $ throwIO (CouldNotCreateIndex r1)
      r2 <- if shardingEnabled
              then putTemplate prx tpl tplName
              else putMapping prx ix mapping base
      unless (statusIsSuccessful (responseStatus r2)) $
        liftIO $ throwIO (CouldNotCreateMapping r2)

  -- workers <- replicateM (unEsPoolSize essPoolSize) $ async $
  --   startBulkWorker cfg env mapping workerSig outChan
  worker <- async $
    startBulkWorker cfg env mapping workerSig outChan

  -- let workers =
  --       replicateConcurrently_ (unEsPoolSize essPoolSize)
  --       $ startBulkWorker cfg env mapping workerSig outChan
  _ <- async $ do
    takeMVar endSig
    putMVar workerSig ()
    -- atomically $ closeTBMQueue q
    -- mapM_ waitCatch workers
    _ <- waitCatch worker
    putMVar endSig ()

  let finalizer = putMVar endSig () >> takeMVar endSig
  return (Scribe (logger inChan) finalizer)
  where
    logger :: forall a
            . LogItem a
           => UNB.InChan (HasData (IndexName v, Value))
           -> Item a
           -> IO ()
    logger inChan i = when (_itemSeverity i >= sev) $
      UNB.writeChan
      inChan
      (HasData (chooseIxn prx ix essIndexSharding i, itemJson' i))

    prx :: Typeable.Proxy v
    prx = Typeable.Proxy

    tplName = toTemplateName prx ixn

    shardingEnabled = case essIndexSharding of
      NoIndexSharding -> False
      _               -> True

    tpl = toIndexTemplate prx (toTemplatePattern prx (ixn <> "-*")) (Just essIndexSettings) [toJSON base]

    base = baseMapping prx mapping

    ixn = fromIndexName prx ix

    itemJson' :: LogItem a => Item a -> Value
    itemJson' i
      | essAnnotateTypes = itemJson verb (TypeAnnotated <$> i)
      | otherwise        = itemJson verb i


data DiscrimLen =
    SendIt
  | StackIt
  deriving Show

data HasData a =
       StopWorker
     | TimeoutTriggered
     | HasData a
     deriving (Show)

startBulkWorker
    :: forall v . (ESVersion v, V5.MonadBH (BH v IO))
    => EsScribeCfg v
    -> BHEnv v
    -> MappingName v
    -> MVar ()
    -- -> UNB.OutChan (IndexName v, Value)
    -> UNB.OutChan (HasData (IndexName v, Value))
    -> IO ()
startBulkWorker _ env mapping dieSignal outChan = do
  -- We need to randomize upload delay
  -- so that workers do not stampede the nodes
  delayTime' <- randomRIO (100, 2000)
  go (delayTime' * milliseconds) [] 0
  where
    milliseconds :: Int
    milliseconds = 1000

    go :: Int -> [V5.BulkOperation] -> Int -> IO ()
    go delayTime xs len = do
      -- result <- race (UNB.readChan outChan) (threadDelay delayTime)
      -- result <- timeout delayTime (UNB.readChan outChan)
      maybeHasData <- UNB.readChan outChan
      case maybeHasData of
        (HasData val) ->
          case discrimLen len of
            SendIt -> do
              newPair <- serializePair val
              _ <- async $ uploadData (newPair : xs)
              dieOrContinue [] 0
            StackIt -> do
              newPair <- serializePair val
              dieOrContinue (newPair : xs) (len+1)
        TimeoutTriggered -> do
          _ <- async $ uploadData xs
          dieOrContinue [] 0
        StopWorker -> do
          _ <- async $ uploadData xs
          return ()

      -- case result of
      --   (Left val) ->
      --     case discrimLen len of
      --       SendIt -> do
      --         newPair <- serializePair val
      --         uploadData (newPair : xs)
      --         dieOrContinue [] 0
      --       StackIt -> do
      --         newPair <- serializePair val
      --         dieOrContinue (newPair : xs) (len+1)
      --   (Right ()) -> do
      --     uploadData xs
      --     dieOrContinue [] 0
      where dieOrContinue newList newLen = do
              dieNow <- tryReadMVar dieSignal
              case dieNow of
                Nothing -> go delayTime newList newLen
                (Just ()) -> return ()

    uploadData :: [V5.BulkOperation] -> IO ()
    uploadData [] = return ()
    uploadData xs = do
      reply <- runBH prx env $ V5.bulk $ V.fromList xs
      case statusCode $ responseStatus reply of
        200 -> return ()
        s -> print s

    serializePair :: (IndexName v, Value) -> IO V5.BulkOperation
    serializePair (indexName, val) = do
      docId <- mkDocId'
      return $
        V5.BulkIndex
        (V5.IndexName (fromIndexName prx indexName))
        (V5.MappingName (fromMappingName prx mapping))
        (V5.DocId docId) val

    discrimLen l
      | l >= 500 = SendIt
      | otherwise = StackIt

    prx :: Typeable.Proxy v
    prx = Typeable.Proxy

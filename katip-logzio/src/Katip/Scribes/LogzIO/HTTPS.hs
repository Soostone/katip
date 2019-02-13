{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}
-- | This is a log scribe that writes logs to logz.io's bulk
-- <https://app.logz.io/#/dashboard/data-sources/Bulk-HTTPS HTTPS
-- API>.
module Katip.Scribes.LogzIO.HTTPS
    ( -- * Scribe construction
      mkLogzIOScribe
      -- * Types
    , BulkAPIError(..)
    , LogzIOScribeConfiguration(..)
    , Scheme(..)
    , APIToken(..)
    , LoggingError(..)
    -- ** Presets for configuration
    , usRegionHost
    , euRegionHost
    , httpsPort
    , httpPort
    , defaultRetryPolicy
    , defaultLogzIOScribeConfiguration
    -- * Internal API exported for testing
    , renderLineTruncated
    , renderLineTruncated'
    , maxPayloadBytes
    , maxLogLineLength
    , BulkBuffer (..)
    , LogAction (..)
    , bufferItem
    , bufferItem'
    , forceFlush
    , Bytes (..)
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import qualified Control.Concurrent.Async   as Async
import qualified Control.Concurrent.STM     as STM
import qualified Control.Error              as E
import qualified Control.Exception.Safe     as EX
import qualified Control.Retry              as Retry
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.HashMap.Strict        as HM
import           Data.Int
import qualified Data.Scientific            as Scientific
import           Data.Semigroup             as Semigroup
import           Data.String                (IsString)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Time                  as Time
import qualified Katip                      as K
import           Katip.Core                 (LocJs (..))
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Client.TLS    as HTTPS
import qualified Network.HTTP.Types         as HTypes
import qualified System.Posix.Types         as POSIX
import qualified URI.ByteString             as URIBS
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------



-- | This is returned when the bulk import was a partial success
data BulkAPIError = BulkAPIError
  { bulkAPIError_malformedLines  :: Int
  -- ^ The number of log lines which are not well-formed JSON. This
  -- indicates a __library bug__. Please file an issue on GitHub.
  , bulkAPIError_successfulLines :: Int
  -- ^ The number of log lines received successfully.
  , bulkAPIError_oversizedLines  :: Int
  -- ^ The number of log lines which exceed the line length
  -- limit. katip-logzio makes a best effort to truncate logs that
  -- exceed the size limit. This probably indicates a __library bug__
  -- and should be reported as an issue on GitHub.
  , bulkAPIError_emptyLogLines   :: Int
  -- ^ The number of log lines which were empty. There isnt' really a
  -- concept of a truly empty log line in katip, so this most likely
  -- indicates a __library bug__ and should be reported as an issue on
  -- GitHub.
  } deriving (Show, Eq)


instance A.FromJSON BulkAPIError where
  parseJSON = A.withObject "BulkAPIError" $ \o -> do
    malformedLines  <- o A..: "malformedLines"
    successfulLines <- o A..: "successfulLines"
    oversizedLines  <- o A..: "oversizedLines"
    emptyLogLines   <- o A..: "emptyLogLines"
    pure $ BulkAPIError
      { bulkAPIError_malformedLines  = malformedLines
      , bulkAPIError_successfulLines = successfulLines
      , bulkAPIError_oversizedLines  = oversizedLines
      , bulkAPIError_emptyLogLines   = emptyLogLines
      }


-------------------------------------------------------------------------------
data LogzIOScribeConfiguration = LogzIOScribeConfiguration
  { logzIOScribeConfiguration_bufferItems   :: Int
  -- ^ Will flush the log buffer if this many items is in the buffer
  -- __or__ 'logzIOScribeConfiguration_bufferTimeout' is reached,
  -- whichever is first
  , logzIOScribeConfiguration_bufferTimeout :: Time.NominalDiffTime
  -- ^ Will flush the buffer if it has been this long since the last
  -- flush __or__ 'logzIOScribeConfiguration_bufferItems' items are
  -- accumulated, whichever is first. NominalDiffTime has a Num
  -- instance, so you can use a literal to specify seconds, e.g. 30 =
  -- 30s.
  , logzIOScribeConfiguration_scheme        :: Scheme
  , logzIOScribeConfiguration_host          :: URIBS.Host
  , logzIOScribeConfiguration_port          :: URIBS.Port
  , logzIOScribeConfiguration_token         :: APIToken
  , logzIOScribeConfiguration_retry         :: Retry.RetryPolicyM IO
  -- ^ How should exceptions during writes be retried?
  , logzIOScribeConfiguration_onError       :: LoggingError -> IO ()
  }


-- | A default configuration:
--
--    * 100 item buffering
--
--    * 30 second buffer timeout
--
--    * US, HTTPS logging endpoint (listener.logz.io:8071)
--
--    * 'defaultRetryPolicy' of 25ms exponential backoff, 5 retries
--
--    * Ignore logging errors
defaultLogzIOScribeConfiguration :: APIToken -> LogzIOScribeConfiguration
defaultLogzIOScribeConfiguration token = LogzIOScribeConfiguration
  { logzIOScribeConfiguration_bufferItems   = 100
  , logzIOScribeConfiguration_bufferTimeout = 30
  , logzIOScribeConfiguration_scheme        = HTTPS
  , logzIOScribeConfiguration_host          = usRegionHost
  , logzIOScribeConfiguration_port          = httpsPort
  , logzIOScribeConfiguration_token         = token
  , logzIOScribeConfiguration_retry         = defaultRetryPolicy
  , logzIOScribeConfiguration_onError       = const (pure ())
  }


-------------------------------------------------------------------------------
-- | You can retrieve your account or sub-account's API token on the
-- <https://app.logz.io/#/dashboard/settings/manage-accounts manage
-- accounts page>. Note that APIToken has an IsString instance,
-- meaning that you can use a string literal with OverloadedStrings
-- enabled.
newtype APIToken = APIToken
  { apiToken :: T.Text
  } deriving (Show, Eq, IsString)


-- | This particular bulk API only supports HTTP or HTTPS. HTTPS is
-- strongly recommended for security reasons.
data Scheme
  = HTTPS
  -- ^ HTTPs should always be used except for local testing.
  | HTTP
  deriving (Show, Eq)


-- | See
-- <https://support.logz.io/hc/en-us/articles/210206365-What-IP-addresses-should-I-open-in-my-firewall-to-ship-logs-to-Logz-io-
-- this> for a list of listeners. This is the US region host,
-- listener.logz.io
usRegionHost :: URIBS.Host
usRegionHost = URIBS.Host "listener.logz.io"


-- | See
-- <https://support.logz.io/hc/en-us/articles/210206365-What-IP-addresses-should-I-open-in-my-firewall-to-ship-logs-to-Logz-io-
-- this> for a list of listeners. This is the EU region host,
-- listener.logz.io
euRegionHost :: URIBS.Host
euRegionHost = URIBS.Host "listener-eu.logz.io"


-- | Logz.io uses port 8071 for HTTPS
httpsPort :: URIBS.Port
httpsPort = URIBS.Port 8071


-- | Logz.io uses port 8070 for HTTP
httpPort :: URIBS.Port
httpPort = URIBS.Port 8070


-- | A reasonable retry policy: exponential backoff with 25ms base
-- delay up to 5 retries, for a total cumulative delay of 775ms.
defaultRetryPolicy :: (Monad m) => Retry.RetryPolicyM m
defaultRetryPolicy = Retry.exponentialBackoff 25000 `mappend` Retry.limitRetries 5

-------------------------------------------------------------------------------
mkLogzIOScribe
  :: LogzIOScribeConfiguration
  -> K.PermitFunc
  -> K.Verbosity
  -> IO K.Scribe
mkLogzIOScribe config permitItem verbosity = do
  -- Accumulating buffer for each item
  bufferState <- STM.newTVarIO (mempty :: BulkBuffer)
  -- Single-slot TMVar when something is ready to write. Katip buffers
  -- for us upstream so we can simplify shutdown and flush guarantees
  -- by synchronously writing when we need to.
  readyMVar <- STM.newEmptyTMVarIO :: IO (STM.TMVar BulkBuffer)

  -- We create a TVar that is set to ShutDown when its time to stop
  killSwitch <- STM.newTVarIO Continue

  -- Create a mutable ref to whether the timer has expired. This lets
  -- us replace the timer when its time to start timing again. We
  -- reset the timer __after__ a write goes out, not as soon as it expires
  timerRef <- STM.newTVarIO =<< STM.registerDelay itemBufferTimeoutMicros

  -- Set up a connection manager for requests
  mgr <- case scheme of
    HTTPS -> HTTPS.newTlsManager
    HTTP  -> HTTP.newManager HTTP.defaultManagerSettings

  -- In STM: resolve an action with the buffers
  let processLogAction :: LogAction BulkBuffer -> STM.STM ()
      processLogAction logAction = case logAction of
        -- No write needed right now
        Buffered !newBuffer -> STM.writeTVar bufferState newBuffer
        -- Time to flush and put excess in the next buffer
        FlushNow !toFlush !newBuffer -> do
          STM.writeTVar bufferState newBuffer
          STM.putTMVar readyMVar toFlush

  -- Recompute the new buffer state from a new log item and take action
  let push :: (K.LogItem a) => K.Item a -> STM.STM ()
      push !logItem = do
        !curBuffer <- STM.readTVar bufferState
        processLogAction (bufferItem itemBufferSize verbosity logItem curBuffer)


  -- A transaction that retries until its time to shutdown
  let waitShutdown :: STM.STM ()
      waitShutdown = do
        killStatus <- STM.readTVar killSwitch
        STM.check (killStatus == ShutDown)

  -- Retry transaction until timer has expired. When it does, take action on the buffer.
  let waitTimer :: STM.STM ()
      waitTimer = do
        -- get the timer we'll be using
        timer <- STM.readTVar timerRef
        -- do not proceed until its ready
        timerExpired <- STM.readTVar timer
        STM.check timerExpired
        -- do not wake again until this message is out the door
        STM.writeTVar timer False
        -- we waited long enough, grab the current buffer
        !curBuffer <- STM.readTVar bufferState
        -- and tell it to flush right now
        processLogAction (forceFlush curBuffer)

  -- run the timer in a loop until its time to shut down
  let timedFlushLoop :: IO ()
      timedFlushLoop = do
        res <- STM.atomically $
          raceAlt
            waitShutdown
            waitTimer
        case res of
          Left ()  -> pure () -- exit loop
          Right () -> timedFlushLoop -- wait for another timer or shutdown

  timedFlusher <- Async.async timedFlushLoop

  -- flush the buffer until its time to shutdown and we're out of items to log
  let flushLoop :: IO ()
      flushLoop = do
        -- hold until something comes in to flush or we're told to stop. If we're told to stop but get a log first, we'll catch it on the next loop
        readyBufferOrDie <- STM.atomically $
           raceAlt
             (STM.takeTMVar readyMVar)
             waitShutdown
        case readyBufferOrDie of
          Right () -> pure () -- we caught shutdown before any new data arrived
          -- There's some logs to write
          Left readyBuffer -> do
            -- flush in IO
            res <- flushBuffer config mgr readyBuffer
            case res of
              Left ex  -> onErrorSafe ex
              Right () -> pure ()
            -- reset the timeout before the next flush on each write
            !newTimer <- STM.registerDelay itemBufferTimeoutMicros
            STM.atomically $ do
              STM.writeTVar timerRef newTimer
            -- start over
            flushLoop
  flusher <- Async.async flushLoop

  -- Blocking call which sends a poison pill to the workers and waits
  -- for them to exit, swallowing exceptions. This follows the
  -- semantics that when katip calls the finalizer, no more log
  -- entries will be sent.
  let shutdown :: IO ()
      shutdown = do
        -- tell workers this is the last loop
        STM.atomically (STM.writeTVar killSwitch ShutDown)
        -- wait for workers to finish
        _ <- Async.waitCatch timedFlusher
        _ <- Async.waitCatch flusher
        -- stop blocking
        pure ()


  pure $ K.Scribe
    { K.liPush = STM.atomically . push
    , K.scribeFinalizer = shutdown
    , K.scribePermitItem = permitItem
    }
  where
    itemBufferSize = logzIOScribeConfiguration_bufferItems config
    itemBufferTimeoutMicros = ndtToMicros (logzIOScribeConfiguration_bufferTimeout config)
    scheme = logzIOScribeConfiguration_scheme config
    -- should this be async?
    onErrorSafe ex = do
      _ <- EX.tryAny (logzIOScribeConfiguration_onError config ex)
      pure ()


-------------------------------------------------------------------------------
-- Match the native resolution of NominalDiffTime, which is excessive
-- for our needs
ndtPicos :: Time.NominalDiffTime -> Int64
ndtPicos = round . (* picos)
  where
    picos :: Time.NominalDiffTime
    picos = 10 ^ (9 :: Int)


-------------------------------------------------------------------------------
ndtToMicros :: Time.NominalDiffTime -> Int
ndtToMicros t = round (((fromIntegral (ndtPicos t)) :: Double) / picosInMicro)
  where
    picosInMicro = 10 ^ (3 :: Int)


-------------------------------------------------------------------------------
-- | An internal mode flag used to shut down worker
-- threads. Internally, we assume htere is no going back from
-- ShutDown.
data KillSwitch =
    Continue
  | ShutDown
  deriving (Eq)


-------------------------------------------------------------------------------
-- | When applied to STM, it will try the left side first and if that
-- retries, the right, and if that retries, start from the beginning.
raceAlt :: (Alternative m) => m a -> m b -> m (Either a b)
raceAlt a b = (Left <$> a) <|> (Right <$> b)


-------------------------------------------------------------------------------
data LoggingError =
    URIError HTTP.HttpException
  -- ^ The URI generated was invalid. Check your configuration
  | RequestError HTTP.HttpException
  -- ^ We encountered an exception while sending the batch request
  | PartialFailure BulkAPIError
  -- ^ Some or all of the request was rejected. Check the logz.io UI
  -- for indexing errors.
  | BadToken
  -- ^ Your API token was rejected.
  | UnknownFailureResponse HTypes.Status LBS.ByteString
  -- ^ An error returned, but it could not be decoded into a
  -- 'BulkAPIError'. This may indicate a __library bug__, which should
  -- be reported to the issue tracker.
  deriving (Show)

-------------------------------------------------------------------------------
flushBuffer
  :: LogzIOScribeConfiguration
  -> HTTP.Manager
  -> BulkBuffer
  -> IO (Either LoggingError ())
flushBuffer config mgr bulkBuffer
  | bulkBuffer_itemCount bulkBuffer <= 0 = pure (Right ())
  | otherwise = do
      E.runExceptT $ do
        req <- E.fmapLT URIError (E.ExceptT (EX.try (configureRequest <$> HTTP.parseRequest uriStr)))
        resp <- E.fmapLT RequestError $ E.ExceptT  $ EX.try $
          Retry.recovering retryPolicy [\_stat -> EX.Handler handleHttpException] $ \_stat ->
            HTTP.httpLbs req mgr
        let respLBS = HTTP.responseBody resp
        let respStatus = HTTP.responseStatus resp
        if HTypes.statusIsSuccessful respStatus
          then pure ()
          else case A.decode @BulkAPIError respLBS of
            Nothing
              | HTypes.statusCode respStatus == 401 -> E.throwE BadToken
              | otherwise -> E.throwE (UnknownFailureResponse respStatus respLBS)
            Just bulkError -> E.throwE (PartialFailure bulkError)
  where
    configureRequest req = req
      { HTTP.method = HTypes.methodPost
      , HTTP.requestBody = HTTP.RequestBodyLBS (BB.toLazyByteString (bulkBuffer_payload bulkBuffer))
      }

    retryPolicy = logzIOScribeConfiguration_retry config
    apiTokenBS = TE.encodeUtf8 (apiToken (logzIOScribeConfiguration_token config))

    handleHttpException :: (Applicative m) => HTTP.HttpException -> m Bool
    handleHttpException _ = pure True

    uriStr = LBS8.unpack (BB.toLazyByteString (URIBS.serializeURIRef uri))

    authority = URIBS.Authority
      { URIBS.authorityUserInfo = Nothing
      , URIBS.authorityHost = logzIOScribeConfiguration_host config
      , URIBS.authorityPort = Just (logzIOScribeConfiguration_port config)
      }

    uri = URIBS.URI
      { URIBS.uriScheme = case logzIOScribeConfiguration_scheme config of
          HTTPS -> URIBS.Scheme "https"
          HTTP  -> URIBS.Scheme "http"
      , URIBS.uriAuthority = Just authority
      , URIBS.uriPath = "/"
      , URIBS.uriQuery = URIBS.Query
          [ ("token", apiTokenBS)
          ]
      , URIBS.uriFragment = Nothing
      }


-------------------------------------------------------------------------------
newtype Bytes = Bytes
  { bytes :: Int64
  } deriving (Show, Eq, Num, Ord, Bounded)


-- | How big of a body can we send? The limit is defined
-- <https://app.logz.io/#/dashboard/data-sources/Bulk-HTTPS here>.
maxPayloadBytes :: Bytes
maxPayloadBytes = 10485760


-- | How long can each serialized payload be (let's assume including
-- the trailing newline). The limit is defined
-- <https://app.logz.io/#/dashboard/data-sources/Bulk-HTTPS here>.
maxLogLineLength :: Bytes
maxLogLineLength = 500000


measureJSONLine :: A.ToJSON a => a -> (BB.Builder, Bytes)
measureJSONLine a = (BB.lazyByteString lbs, Bytes (LBS.length lbs))
  where
    lbs = A.encode a <> "\n"


-- | Fully-rendered JSON object for an item
fullItemObject :: K.LogItem a => K.Verbosity -> K.Item a -> A.Object
fullItemObject verbosity item = HM.fromList
  [ "app" A..= K._itemApp item
  , "env" A..= K._itemEnv item
  , "sev" A..= K._itemSeverity item
  , "thread" A..= K.getThreadIdText (K._itemThread item)
  , "host" A..= K._itemHost item
  , "pid" A..= pidInt
  , "data" A..= annotateKeys (K.payloadObject verbosity (K._itemPayload item))
  -- Slight deviation, logz.io uses "message" instead of "msg"
  , "message" A..= TB.toLazyText (K.unLogStr (K._itemMessage item))
  -- Another slight deviation, logz.io uses "@timestamp" instead of
  -- "at". Note, your logs should be sent roughly close to when they
  -- are created. They are assigned to indexes based on index date, so
  -- time searches act strange if you backfill too far from the
  -- past. They seem to support 3 decimal places of
  -- precision. Formatting like this requires time 1.8.0.2, which is
  -- reflected in the cabal file.
  , "@timestamp" A..= A.String (T.pack (Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%03QZ" (K._itemTime item)))
  , "ns" A..= K._itemNamespace item
  , "loc" A..= (LocJs <$> K._itemLoc item)
  ]
  where
    POSIX.CPid pidInt = K._itemProcess item


-- | A version of 'renderLine' which renders a line and stays under
-- the maximum line size of 500,000 bytes. If the default rendering is
-- too large, the log will be reduced to a timestamp and a potentially
-- truncated message.
renderLineTruncated :: K.LogItem a => K.Verbosity -> K.Item a -> (BB.Builder, Bytes)
renderLineTruncated = renderLineTruncated' maxLogLineLength


-- | A generalized renderLineTruncated that takes a custom line length
-- limit. This is exclusively for testing.
renderLineTruncated'
  :: K.LogItem a
  => Bytes
  -- ^ Custom max log line length. Be careful, too low and no amount
  -- of shrinking can get under the size, breaking the invariant. For
  -- the production limit, we are guraanteed always able to come in
  -- under the limit.
  -> K.Verbosity
  -> K.Item a
  -> (BB.Builder, Bytes)
renderLineTruncated' customMaxLogLineLength verbosity item =
  if fullSize <= customMaxLogLineLength
    then (fullLine, fullSize)
    else (fallbackLine, fallbackSize)
  where
    fullObject = fullItemObject verbosity item
    (fullLine, fullSize) = measureJSONLine fullObject
    -- only the absolutely necessary keys, with message stripped out
    -- to help us calculate how much of the message we can keep. these
    -- are lazily evaluated and as such won't be computed unless the
    -- item is too big
    blankObject :: A.Object
    blankObject = HM.fromList
      [ "message" A..= A.String "" -- we'll start with a blank message
      , "@timestamp" A..= K._itemTime item
      ]
    (_, blankObjectSize) = measureJSONLine blankObject
    messageBytesAllowed = maxLogLineLength - blankObjectSize
    (fallbackLine, fallbackSize) = measureJSONLine fallbackObject
    fallbackObject :: A.Object
    fallbackObject = HM.fromList
      [ "message" A..= A.toJSON (TL.take (bytes messageBytesAllowed) (TB.toLazyText (K.unLogStr (K._itemMessage item))))
      , "@timestamp" A..= A.toJSON (K._itemTime item)
      ]


data BulkBuffer = BulkBuffer
  { bulkBuffer_bytesUsed :: !Bytes
  , bulkBuffer_payload   :: !BB.Builder
  , bulkBuffer_itemCount :: !Int
  }


instance Semigroup.Semigroup BulkBuffer where
  (BulkBuffer bytesUsedA bufferA itemCountA) <>
    (BulkBuffer bytesUsedB bufferB itemCountB) = BulkBuffer
      (bytesUsedA + bytesUsedB)
      (bufferA <> bufferB)
      (itemCountA + itemCountB)


instance Monoid BulkBuffer where
  mempty = BulkBuffer 0 mempty 0
  mappend = (<>)


data LogAction buf =
    Buffered
      buf
      -- ^ New buffer
  | FlushNow
      buf
      -- ^ Buffer to flush
      buf
      -- ^ New buffer

bufferItem
  :: K.LogItem a
  => Int
  -- ^ Maximum items before flushing
  -> K.Verbosity
  -> K.Item a
  -> BulkBuffer
  -> LogAction BulkBuffer
bufferItem = bufferItem' maxPayloadBytes maxLogLineLength


-- | An internal version with a configurable max item size and max
-- message size for testing. Be careful with this: if you set the
-- values too low, some of the guarantees about reducability of
-- messages break down.
bufferItem'
  :: (K.LogItem a)
  => Bytes
  -- ^ Max payload size
  -> Bytes
  -- ^ Max item size
  -> Int
  -- ^ Maximum items before flushing
  -> K.Verbosity
  -> K.Item a
  -> BulkBuffer
  -> LogAction BulkBuffer
bufferItem' customMaxPayload customMaxItem maxItems verb item bulkBuffer =
  let (encodedLine, spaceNeeded) = renderLineTruncated' customMaxItem verb item
      newBytesUsed = bulkBuffer_bytesUsed bulkBuffer + spaceNeeded
      newItemCount = bulkBuffer_itemCount bulkBuffer + 1
  in if newItemCount >= maxItems || newBytesUsed >= customMaxPayload
       then FlushNow
              bulkBuffer
              BulkBuffer
                { bulkBuffer_bytesUsed = spaceNeeded
                , bulkBuffer_payload = encodedLine
                , bulkBuffer_itemCount = 1
                }
       else Buffered $
         BulkBuffer
           { bulkBuffer_bytesUsed = newBytesUsed
           , bulkBuffer_payload = bulkBuffer_payload bulkBuffer <> encodedLine
           , bulkBuffer_itemCount = newItemCount
           }


-- | When time has run out on a flush, splits the buffer. n.b. this
-- will always return 'FlushNow' with an empty new buffer.
forceFlush :: (Monoid buf) => buf -> LogAction buf
forceFlush buf = FlushNow buf mempty


-------------------------------------------------------------------------------
-- Annotation borrowed from katip-elasticsearch. There are fixed
-- fields in katip logs which should stay the same type forever and
-- thus won't need annotation. However, any code in userland may
-- choose to add akey to their log's metadata with a type that's
-- incompatible with the mapping. If the first value that's picked up
-- restrictive (e.g. a long), subsequent values will be rejected. By
-- differentiating types by their name, this is guaranteed not to
-- happen.

annotateValue :: A.Value -> A.Value
annotateValue (A.Object o) = A.Object (annotateKeys o)
annotateValue (A.Array a)  = A.Array (annotateValue <$> a)
annotateValue x            = x


annotateKeys :: A.Object -> A.Object
annotateKeys = HM.fromList . map go . HM.toList
  where
    go (k, A.Object o) = (k, A.Object (annotateKeys o))
    go (k, A.Array a)  = (k, A.Array (annotateValue <$> a))
    go (k, s@(A.String _)) = (k <> stringAnn, s)
    go (k, n@(A.Number sci)) = if Scientific.isFloating sci
                               then (k <> doubleAnn, n)
                               else (k <> longAnn, n)
    go (k, b@(A.Bool _)) = (k <> booleanAnn, b)
    go (k, A.Null) = (k <> nullAnn, A.Null)


-------------------------------------------------------------------------------
-- Annotation Constants
-------------------------------------------------------------------------------


stringAnn :: T.Text
stringAnn = "::s"

doubleAnn :: T.Text
doubleAnn = "::d"

longAnn :: T.Text
longAnn = "::l"

booleanAnn :: T.Text
booleanAnn = "::b"

nullAnn :: T.Text
nullAnn = "::n"

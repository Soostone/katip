-- | This is a log scribe that writes logs to logz.io's bulk
-- <https://app.logz.io/#/dashboard/data-sources/Bulk-HTTPS HTTPS
-- API>.
module Katip.Scribes.LogzIO.HTTPS
    ( -- * Scribe construction
      mkLogzIOScribe
      -- * Types
    , BulkAPIError(..)
    , LogzIOScribeConfiguration(..)
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM   as STM
import qualified Data.Aeson               as A
import qualified Data.ByteString.Builder  as BB
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.HashMap.Strict      as HM
import           Data.Int
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Builder   as TB
import qualified Data.Time                as Time
import qualified Katip                    as K
import           Katip.Core               (LocJs (..))
import qualified System.Posix.Types       as POSIX
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
  -- accumulated, whichever is first.
  --TODO: more
  }

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
  -- for us so we can simplify shutdown and flush guarantees this way
  readyMVar <- STM.newEmptyTMVarIO :: IO (STM.TMVar BulkBuffer)

  -- We create a TVar that is set to ShutDown when its time to stop
  killSwitch <- STM.newTVarIO Continue

  -- Create a mutable ref to whether the timer has expired. This lets
  -- us replace the timer when its time to start timing again. We
  -- reset the timer __after__ a write goes out, not as soon as it expires
  timerRef <- STM.atomically $ do
    -- If there's never been a log, no need to start the timer
    never <- STM.newTVar False
    STM.newTVar never


  -- on every push, extend deadline
  -- thread waits for deadline and pushes. need a way to be atomic though, cancelthread is heavy handed
  --TODO: strictness

  -- in STM: resolve an action with the buffers
  let processLogAction logAction = case logAction of
        Buffered newBuffer -> STM.writeTVar bufferState newBuffer
        FlushNow toFlush newBuffer -> do
          STM.writeTVar bufferState newBuffer
          STM.putTMVar readyMVar toFlush

  -- In recompute the new buffer state from a new log item and take action
  let push logItem = do
        curBuffer <- STM.readTVar bufferState
        processLogAction (bufferItem itemBufferSize verbosity logItem curBuffer)


  -- A transaction that retries until its time to shutdown
  let waitShutdown = do
        killStatus <- STM.readTVar killSwitch
        STM.check (killStatus == ShutDown)

  --TODO: exception handling

  -- Retry transaction until timer has expired
  let waitTimer = do
        -- get the timer we'll be using
        timer <- STM.readTVar timerRef
        -- do not proceed until its ready
        timerExpired <- STM.readTVar timer
        STM.check timerExpired
        -- do not wake again until this message is out the door
        STM.writeTVar timer False
        -- we waited long enough, grab the current buffer
        curBuffer <- STM.readTVar bufferState
        -- and tell it to flush right now
        processLogAction (forceFlush curBuffer)

  -- loop until its time to shut down
  let timedFlushLoop = do
        res <- STM.atomically $
          raceAlt
            waitShutdown
            waitTimer
        case res of
          Left ()  -> pure () -- exit loop
          Right () -> timedFlushLoop -- wait for another timer or shutdown

  timedFlusher <- Async.async timedFlushLoop

  let flushLoop = do
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
            flushBuffer readyBuffer
            -- reset the timeout before the next flush on each write
            newTimer <- STM.registerDelay itemBufferTimeoutMicros
            STM.atomically $ do
              STM.writeTVar timerRef newTimer
            -- start over
            flushLoop
  flusher <- Async.async flushLoop

  let shutdown = do
        -- tell workers this is the last loop
        STM.atomically (STM.writeTVar killSwitch ShutDown)
        -- wait for workers to finish
        _ <- Async.waitCatch timedFlusher
        _ <- Async.waitCatch flusher
        -- stop blocking
        pure ()


  pure $ K.Scribe
    { liPush = STM.atomically . push
    , scribeFinalizer = shutdown
    , scribePermitItem = permitItem
    }
  where
    itemBufferSize = logzIOScribeConfiguration_bufferItems config
    itemBufferTimeoutMicros = ndtToMicros (logzIOScribeConfiguration_bufferTimeout config)


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
data KillSwitch =
    Continue
  | ShutDown
  deriving (Eq)


-------------------------------------------------------------------------------
raceAlt :: (Alternative m) => m a -> m b -> m (Either a b)
raceAlt a b = (Left <$> a) <|> (Right <$> b)


-------------------------------------------------------------------------------
-- TODO: noop on empty
flushBuffer :: BulkBuffer -> IO ()
flushBuffer = undefined

-------------------------------------------------------------------------------
newtype Bytes = Bytes
  { bytes :: Int64
  } deriving (Show, Eq, Num, Ord)


-- | How big of a body can we send? The limit is defined
-- <https://app.logz.io/#/dashboard/data-sources/Bulk-HTTPS here>.
maxPayloadBytes :: Bytes
maxPayloadBytes = 10485760


-- | How long can each serialized payload be (let's assume including
-- the trailing newline). The limit is defined
-- <https://app.logz.io/#/dashboard/data-sources/Bulk-HTTPS here>.
maxLogLineLength :: Bytes
maxLogLineLength = 500000


measureJSON :: A.ToJSON a => a -> (BB.Builder, Bytes)
measureJSON a = (BB.lazyByteString lbs, Bytes (LBS.length lbs))
  where
    lbs = A.encode a


-- | Fully-rendered JSON object for an item
fullItemObject :: K.LogItem a => K.Verbosity -> K.Item a -> A.Object
fullItemObject verbosity item = HM.fromList
  [ "app" A..= K._itemApp item
  , "env" A..= K._itemEnv item
  , "sev" A..= K._itemSeverity item
  , "thread" A..= K.getThreadIdText (K._itemThread item)
  , "host" A..= K._itemHost item
  , "pid" A..= pidInt
  , "data" A..= K.payloadObject verbosity (K._itemPayload item)
  -- Slight deviation, logz.io uses "message" instead of "msg"
  , "message" A..= TB.toLazyText (K.unLogStr (K._itemMessage item))
  -- Another slight deviation, logz.io uses "@timestamp" instead
  -- of "at". Note, your logs should be sent roughly close to when
  -- they are created. They are assigned to indexes based on index
  -- date, so time searches act strange if you backfill too far
  -- from the past.
  , "@timestamp" A..= K._itemTime item --TODO: explicit formatting as per spec?
  , "ns" A..= K._itemNamespace item
  , "loc" A..= (LocJs <$> K._itemLoc item)
  ]
  where
    POSIX.CPid pidInt = K._itemProcess item

--TODO: property test

-- | A version of 'renderLine' which renders a line and stays under
-- the maximum line size of 500,000 bytes. If the default rendering is
-- too large, the log will be reduced to a timestamp and a potentially
-- truncated message.
renderLineTruncated :: K.LogItem a => K.Verbosity -> K.Item a -> (BB.Builder, Bytes)
renderLineTruncated verbosity item =
  if fullSize <= maxLogLineLength
    then (fullLine, fullSize)
    else (fallbackLine, fallbackSize)
  where
    fullObject = fullItemObject verbosity item
    (fullLine, fullSize) = measureJSON fullObject
    -- only the absolutely necessary keys, with message stripped out
    -- to help us calculate how much of the message we can keep. these
    -- are lazily evaluated and as such won't be computed unless the
    -- item is too big
    blankObject :: A.Object
    blankObject = HM.fromList
      [ "message" A..= A.String "" -- we'll start with a blank message
      , "@timestamp" A..= K._itemTime item
      ]
    (_, blankObjectSize) = measureJSON blankObject
    messageBytesAllowed = maxLogLineLength - blankObjectSize
    (fallbackLine, fallbackSize) = measureJSON fallbackObject
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


instance Semigroup BulkBuffer where
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
bufferItem maxItems verb item bulkBuffer =
  let (encodedLine, spaceNeeded) = renderLineTruncated verb item
      newBytesUsed = bulkBuffer_bytesUsed bulkBuffer + spaceNeeded
      newItemCount = bulkBuffer_itemCount bulkBuffer + 1
  in if newItemCount >= maxItems || newBytesUsed >= maxPayloadBytes
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

{-# LANGUAGE OverloadedStrings #-}
module Katip.Tests.Scribes.LogzIO.HTTPS
    ( tests
    ) where


-------------------------------------------------------------------------------
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TSem
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson                  as A
import qualified Data.ByteString.Builder     as BB
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Lazy.Char8  as LBS8
import qualified Data.HashMap.Strict         as HM
import           Data.Int
import           Data.Scientific
import           Data.Text                   (Text)
import           Data.Time
import           Data.Time.Clock.POSIX
import qualified Data.Vector                 as V
import           Hedgehog                    as HH
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Katip
import           Katip.Core
import           Language.Haskell.TH         (Loc (..))
import           Network.HostName
import qualified Network.HTTP.Types.Status   as HTypes
import qualified Network.Wai.Handler.Warp    as Warp
import           System.Posix
import           System.Timeout
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit
import           URI.ByteString
import qualified Web.Scotty                  as Scotty
-------------------------------------------------------------------------------
import           Katip.Scribes.LogzIO.HTTPS
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Katip.Scribes.LogzIO.HTTP"
  [ bufferItemTests
  , renderLineTruncatedTests
  , scribeTests
  ]


-------------------------------------------------------------------------------
-- We bind to a system port so we use a lock on these tests to avoid interleaved, concurrent requests
scribeTests :: TestTree
scribeTests = withLock $ \mkSem -> testGroup "scribe"
  [ testCase "writes well-formed log items as lines to the given API" $ do
      sem <- mkSem
      withServer sem $ \receivedMessages -> do
        withScribe (\cfg -> cfg { logzIOScribeConfiguration_bufferTimeout = 1}) $ \scribe -> do
          (liPush scribe) (genericLog "a message")

          requestLog <- timeoutFailure (20 * second) (atomically (readTQueue receivedMessages))
          requestLog_token requestLog @?= testAPIToken
          let logs = requestLog_logs requestLog
          length logs @?= 1
          let firstLog = head logs
          receivedLog_message firstLog @?= "a message"
  , testCase "respects the items limit" $ do
      sem <- mkSem
      withServer sem $ \receivedMessages -> do
        withScribe (\cfg -> cfg { logzIOScribeConfiguration_bufferItems = 1, logzIOScribeConfiguration_bufferTimeout = 1}) $ \scribe -> do
          (liPush scribe) (genericLog "message 1")
          (liPush scribe) (genericLog "message 2")

          log1 <- timeoutFailure (20 * second) (atomically (readTQueue receivedMessages))
          requestLog_token log1 @?= testAPIToken
          let logs1 = requestLog_logs log1
          length logs1 @?= 1
          let firstLog1 = head logs1
          receivedLog_message firstLog1 @?= "message 1"

          log2 <- timeoutFailure (20 * second) (atomically (readTQueue receivedMessages))
          let logs2 = requestLog_logs log2
          length logs2 @?= 1
          let firstLog2 = head logs2
          receivedLog_message firstLog2 @?= "message 2"
  , testCase "respects the time limit" $ do
      sem <- mkSem
      withServer sem $ \receivedMessages -> do
        withScribe (\cfg -> cfg { logzIOScribeConfiguration_bufferItems = 2, logzIOScribeConfiguration_bufferTimeout = 1}) $ \scribe -> do
          (liPush scribe) (genericLog "a message")
          log1 <- timeoutFailure (20 * second) (atomically (readTQueue receivedMessages))
          requestLog_token log1 @?= testAPIToken
          let logs1 = requestLog_logs log1
          length logs1 @?= 1
          let firstLog1 = head logs1
          receivedLog_message firstLog1 @?= "a message"
  , testCase "reports errors" $ do
      sem <- mkSem
      let badToken = APIToken "badtoken"
      errorSignal <- newEmptyTMVarIO
      withServer sem $ \_receivedMessages -> do
        withScribe (\cfg -> cfg { logzIOScribeConfiguration_bufferItems = 1, logzIOScribeConfiguration_bufferTimeout = 1, logzIOScribeConfiguration_token = badToken, logzIOScribeConfiguration_onError = void . atomically . tryPutTMVar errorSignal}) $ \scribe -> do
          (liPush scribe) (genericLog "a message")
          loggingError <- timeoutFailure (20 * second) (atomically (takeTMVar errorSignal))
          case loggingError of
            BadToken -> pure ()
            _ -> assertFailure ("Expected BadToken error but got " <> show loggingError)
  , testCase "flushes all queued logs on close" $ do
      sem <- mkSem
      withServer sem $ \receivedMessages -> do
        withScribe (\cfg -> cfg { logzIOScribeConfiguration_bufferItems = 100, logzIOScribeConfiguration_bufferTimeout = 100}) $ \scribe -> do
          (liPush scribe) (genericLog "message 1")
          (liPush scribe) (genericLog "message 2")
        messages <- atomically (flushTQueue receivedMessages)
        let allLogs = receivedLog_message <$> mconcat (requestLog_logs <$> messages)
        allLogs @?= ["message 1", "message 2"]
  ]
  where
    second = 1000000
    timeoutFailure waitTime f = do
      res <- timeout waitTime f
      case res of
        Nothing -> assertFailure ("Operation timed out after " <> show waitTime <> "us")
        Just x -> pure x
    withLock = withResource (atomically (newTSem 1)) (const (return ()))
    withServer sem f = bracket_ (atomically (waitTSem sem)) (atomically (signalTSem sem)) $ do
      receivedMessages <- atomically newTQueue
      withAsync (startServer receivedMessages) $ \_async -> f receivedMessages
    testAPIToken = "bigSECRET"
    genericLog message = Item
      { _itemApp       = "katip-logzio"
      , _itemEnv       = "test"
      , _itemSeverity  = InfoS
      , _itemThread    = ThreadIdText "42"
      , _itemHost      = "localhost"
      , _itemProcess   = 111
      , _itemPayload   = ()
      , _itemMessage   = message
      , _itemTime      = mkUTCTime 2019 1 2 3 4 5
      , _itemNamespace = "test"
      , _itemLoc       = Nothing
      }
    startServer receivedMessages = Scotty.scottyOpts serverOpts $ do
      Scotty.post "/" $ do
        token <- APIToken <$> Scotty.param "token"
        if token == testAPIToken
          then do
            bodyLines <- LBS8.lines <$> Scotty.body
            receivedLogs <- mapM (either error pure . A.eitherDecode) bodyLines
            let requestLog = RequestLog
                  { requestLog_token = token
                  , requestLog_logs = receivedLogs
                  }
            liftIO (atomically (writeTQueue receivedMessages requestLog))
          else Scotty.status HTypes.unauthorized401
    serverOpts = Scotty.Options
      { Scotty.verbose = 0
      , Scotty.settings = Warp.setPort 1337 Warp.defaultSettings
      }
    withScribe modConfig =
      bracket (mkLogzIOScribe (modConfig scribeConfig) (const (pure True)) V3) scribeFinalizer
    scribeConfig = LogzIOScribeConfiguration
      { logzIOScribeConfiguration_bufferItems   = 5
      , logzIOScribeConfiguration_bufferTimeout = 10
      , logzIOScribeConfiguration_scheme        = HTTP
      , logzIOScribeConfiguration_host          = Host "127.0.0.1"
      , logzIOScribeConfiguration_port          = Port 1337
      , logzIOScribeConfiguration_token         = testAPIToken
      , logzIOScribeConfiguration_retry         = defaultRetryPolicy
      , logzIOScribeConfiguration_onError       = const (pure ())
      }



data RequestLog = RequestLog
  { requestLog_token :: APIToken
  , requestLog_logs  :: [ReceivedLog]
  }


-- | Simplified log that only parses the fields we want to assert on
data ReceivedLog = ReceivedLog
  { receivedLog_message :: Text
  }


instance FromJSON ReceivedLog where
  parseJSON = withObject "ReceivedLog" $ \o -> do
    message <- o .: "message"
    pure $ ReceivedLog
      { receivedLog_message = message
      }


-------------------------------------------------------------------------------
bufferItemTests :: TestTree
bufferItemTests = testGroup "bufferItem"
  [ testProperty "0 or negative maximum items always flushes immediately" $ property $ do
      item <- forAllWith showSimplePayloadItem genSimplePayloadItem
      verbosity <- forAll genVerbosity
      maxItems <- forAll (Gen.int (Range.linear minBound 0))
      bulkBuffer <- forAllWith inspectBulkBuffer genBulkBuffer
      let logAction = bufferItem' reducedMaxPayloadBytes reducedMaxLogLineLength maxItems verbosity item bulkBuffer
      assertFlushNow logAction
  , testProperty "never buffers too many items" $ property $ do
      item <- forAllWith showSimplePayloadItem genSimplePayloadItem
      verbosity <- forAll genVerbosity
      maxItems <- forAll (Gen.int (Range.linear 0 100))
      bulkBuffer <- forAllWith inspectBulkBuffer genBulkBuffer
      let logAction = bufferItem' reducedMaxPayloadBytes reducedMaxLogLineLength maxItems verbosity item bulkBuffer
      case logAction of
        Buffered newBuffer -> HH.assert (bulkBuffer_itemCount newBuffer <= maxItems)
        _                  -> pure ()
  , testProperty "never produces too many bytes" $ property $ do
      item <- forAllWith showSimplePayloadItem genSimplePayloadItem
      verbosity <- forAll genVerbosity
      maxItems <- forAll (Gen.int (Range.linear 0 100))
      bulkBuffer <- forAllWith inspectBulkBuffer genBulkBuffer
      let logAction = bufferItem' reducedMaxPayloadBytes reducedMaxLogLineLength maxItems verbosity item bulkBuffer
      case logAction of
        Buffered newBuffer -> HH.assert (bulkBuffer_bytesUsed newBuffer <= reducedMaxPayloadBytes)
        _ -> pure ()
  , testProperty "never flushes more than 1 item" $ property $ do
      item <- forAllWith showSimplePayloadItem genSimplePayloadItem
      verbosity <- forAll genVerbosity
      maxItems <- forAll (Gen.int (Range.linear minBound 0))
      bulkBuffer <- forAllWith inspectBulkBuffer genBulkBuffer
      let logAction = bufferItem' reducedMaxPayloadBytes reducedMaxLogLineLength maxItems verbosity item bulkBuffer
      case logAction of
        FlushNow _ newBuffer -> bulkBuffer_itemCount newBuffer === 1
        _                    -> pure ()
  , testProperty "flushes when the buffer is exactly at the limit" $ property $ do
      maxItems <- forAll (Gen.int (Range.linear 1 20))
      item <- forAllWith showSimplePayloadItem genSimplePayloadItem
      bulkBuffer <- forAllWith inspectBulkBuffer ((\b -> b { bulkBuffer_itemCount = maxItems - 1 }) <$> genBulkBuffer)
      verbosity <- forAll genVerbosity
      let logAction = bufferItem' maxBound maxBound maxItems verbosity item bulkBuffer
      case logAction of
        FlushNow _ _ -> pure ()
        Buffered _   -> fail "Expected a FlushNow but got a Buffered"
  ]
  where
    assertFlushNow (FlushNow _ _) = pure ()
    assertFlushNow (Buffered _)   = fail ("Expceted FlushNow but got a Buffered")
    inspectBulkBuffer (BulkBuffer used _ itemCount) =
      "BulkBuffer (" <> show used <> ") ... " <> show itemCount


-------------------------------------------------------------------------------
-- | Reduced max log line length for more space and time efficient tests
reducedMaxLogLineLength :: Bytes
reducedMaxLogLineLength = Bytes (bytes maxLogLineLength `div` limitScalingFactor)


reducedMaxPayloadBytes :: Bytes
reducedMaxPayloadBytes = Bytes (bytes maxPayloadBytes `div` limitScalingFactor)


limitScalingFactor :: Int64
limitScalingFactor = 10

-------------------------------------------------------------------------------
showSimplePayloadItem :: Item SimpleLogPayload -> String
showSimplePayloadItem item =
  show (item { _itemPayload = [(k, toJSON v) | (k, AnyLogPayload v) <- unSimpleLogPayload (_itemPayload item)]})


-------------------------------------------------------------------------------
renderLineTruncatedTests :: TestTree
renderLineTruncatedTests = testGroup "renderLineTruncated"
  [ testProperty "always produces an accurate byte count" $ property $ do
      item <- forAllWith showSimplePayloadItem genSimplePayloadItem
      verbosity <- forAll genVerbosity
      let (builder, Bytes byteCount) = renderLineTruncated' reducedMaxLogLineLength verbosity item
      builderLength builder === byteCount
  , testProperty "always produces a payload smaller than the line limit" $ property $ do
      let tinyMaxLineLength = 1024 -- set a really low one for speed
      let lowerBound = fromIntegral (bytes tinyMaxLineLength - 100)
      let upperBound = fromIntegral (bytes tinyMaxLineLength + 100)
      bigMessage <- forAll (genLogStr' (Range.linear lowerBound upperBound))
      item <- (\item -> item { _itemMessage = bigMessage}) <$> forAllWith showSimplePayloadItem genSimplePayloadItem
      verbosity <- forAll genVerbosity
      let (_, byteCount) = renderLineTruncated' tinyMaxLineLength verbosity item
      HH.assert (byteCount <= reducedMaxLogLineLength)
  ]


-------------------------------------------------------------------------------
builderLength :: BB.Builder -> Int64
builderLength = LBS.length . BB.toLazyByteString

-------------------------------------------------------------------------------
genVerbosity :: Gen Verbosity
genVerbosity = Gen.enumBounded


-------------------------------------------------------------------------------
genBulkBuffer ::  Gen BulkBuffer
genBulkBuffer = do
  bytesUsed <- genBytes
  -- generating a large bytestring string with hedghog seems quite slow, instead we'll generate a smaller string and replicate and merge it
  payloadMultiplier <- Gen.int (Range.linear 1 100)
  payloadSegment <- genBuilder (Range.linear 0 (round (toRational (bytes reducedMaxPayloadBytes) * 0.05)))
  let payload = mconcat (replicate payloadMultiplier payloadSegment)
  itemCount <- Gen.int (Range.linear 0 maxBound)
  pure $ BulkBuffer
    { bulkBuffer_bytesUsed = bytesUsed
    , bulkBuffer_payload = payload
    , bulkBuffer_itemCount = itemCount
    }


-------------------------------------------------------------------------------
genBytes :: Gen Bytes
genBytes = Bytes <$> Gen.int64 (Range.linear 0 maxBound)


-------------------------------------------------------------------------------
genBuilder :: Range Int -> Gen BB.Builder
genBuilder size = BB.byteString <$> Gen.bytes size


-------------------------------------------------------------------------------
genSimplePayloadItem :: Gen (Item SimpleLogPayload)
genSimplePayloadItem = do
  app <- genNamespace
  env <- genEnvironment
  severity <- genSeverity
  thread <- genThreadIdText
  host <- genHostName
  process <- genProcessID
  payload <- genSimpleLogPayload
  message <- genLogStr
  time <- genUTCTime
  namespace <- genNamespace
  loc <- Gen.maybe genLoc
  pure $ Item
    { _itemApp       = app
    , _itemEnv       = env
    , _itemSeverity  = severity
    , _itemThread    = thread
    , _itemHost      = host
    , _itemProcess   = process
    , _itemPayload   = payload
    , _itemMessage   = message
    , _itemTime      = time
    , _itemNamespace = namespace
    , _itemLoc       = loc
    }


genNamespace :: Gen Namespace
genNamespace = Namespace <$> Gen.list (Range.linear 0 20) genSmallText


genEnvironment :: Gen Environment
genEnvironment = Environment <$> genSmallText


genSeverity :: Gen Severity
genSeverity = Gen.enumBounded


genThreadIdText :: Gen ThreadIdText
genThreadIdText = ThreadIdText <$> genSmallText


genHostName :: Gen HostName
genHostName = genSmallString


genProcessID :: Gen ProcessID
genProcessID = Gen.enumBounded


genLogStr :: Gen LogStr
genLogStr = genLogStr' (Range.linear 0 1024)


genLogStr' :: Range Int -> Gen LogStr
genLogStr' sizeRange = ls <$> Gen.text sizeRange Gen.unicode


genUTCTime :: Gen UTCTime
genUTCTime = posixSecondsToUTCTime . fromIntegral
  <$> Gen.int (Range.linear 0 maxBound)


genLoc :: Gen Loc
genLoc = Loc
  <$> genSmallString
  <*> genSmallString
  <*> genSmallString
  <*> genCharPos
  <*> genCharPos
  where
    genCharPos = (,)
      <$> Gen.int (Range.linear 0 maxBound)
      <*> Gen.int (Range.linear 0 maxBound)


genSimpleLogPayload :: Gen SimpleLogPayload
genSimpleLogPayload = SimpleLogPayload
  <$> Gen.list (Range.linear 0 20) genPair
  where
    genPair = (,)
      <$> genSmallText
      <*> genAnyLogPayload


genAnyLogPayload :: Gen AnyLogPayload
genAnyLogPayload = AnyLogPayload
  <$> genValue


genValue :: Gen Value
genValue = Gen.recursive Gen.choice nonRecursiveGens recursiveGens
  where
    nonRecursiveGens =
      [ pure A.Null
      , A.String <$> genSmallText
      , A.Number <$> genScientific
      , A.Bool <$> Gen.bool
      ]
    recursiveGens =
      [ A.Object <$> genObject
      , A.Array <$> genArray
      ]
    genObject = HM.fromList <$> Gen.list (Range.linear 0 20) genPair
      where
        genPair = (,) <$> genSmallText <*> genValue
    genArray = V.fromList <$>
      Gen.list (Range.linear 0 20) genValue



genScientific :: Gen Scientific
genScientific = scientific
  <$> Gen.integral (Range.linear (-999999) 999999)
  <*> Gen.int (Range.linear (-999) (999))


genSmallText :: Gen Text
genSmallText = Gen.text (Range.linear 0 128) Gen.unicode


genSmallString :: Gen String
genSmallString = Gen.string (Range.linear 0 128) Gen.unicode


-------------------------------------------------------------------------------
mkUTCTime :: Integer -> Int -> Int -> DiffTime -> DiffTime -> DiffTime -> UTCTime
mkUTCTime y m d hr mn s = UTCTime day dt
  where
    day = mkDay y m d
    dt = s + 60 * mn + 60 * 60 * hr


-------------------------------------------------------------------------------
mkDay :: Integer -> Int -> Int -> Day
mkDay y m d = day
  where
    Just day = fromGregorianValid y m d

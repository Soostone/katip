{-# LANGUAGE OverloadedStrings #-}
module Katip.Tests.Scribes.Datadog.TCP
    ( tests
    ) where


-------------------------------------------------------------------------------
import           Control.Concurrent.Async
import qualified Control.Exception.Safe    as EX
import qualified Data.Aeson                as A
import qualified Data.Conduit              as C
import qualified Data.Conduit.Binary       as CB
import qualified Data.Conduit.List         as CL
import qualified Data.Conduit.Network      as CN
import           Data.Foldable             (toList)
import qualified Data.HashMap.Strict       as HM
import           Data.IORef
import           Data.Sequence             ((|>))
import           Data.Text
import           Test.Tasty
import           Test.Tasty.HUnit
-------------------------------------------------------------------------------
import           Katip
import           Katip.Scribes.Datadog.TCP
-------------------------------------------------------------------------------



tests :: TestTree
tests = testGroup "Katip.Scribes.Datadog.TCP"
  [ testCase "logs well-formed messages in order" $ do
       logs <- collectLogs $ do
         logItem (sl "foo" ("bar" :: Text)) "mynamespace" noLoc InfoS "a message"
       logs @?=
         [ CapturedLog
             { capturedLog_namespace = Namespace ["katip-datadog-tests", "mynamespace"]
             , capturedLog_data = A.Object (HM.singleton "foo" (A.String "bar"))
             , capturedLog_sev = InfoS
             , capturedLog_message = "a message"
             }
         ]
  ]
  where
    noLoc = Nothing


-------------------------------------------------------------------------------
-- | A log type parsed out of the fake datadog agent's buffer
data CapturedLog = CapturedLog
  { capturedLog_namespace :: Namespace
  , capturedLog_data      :: A.Value
  , capturedLog_sev       :: Severity
  , capturedLog_message   :: Text
  } deriving (Show, Eq)


instance A.FromJSON CapturedLog where
  parseJSON = A.withObject "CapturedLog" $ \o -> do
    message <- o A..: "message"
    sev <- o A..: "severity"
    dta <- o A..: "data"
    namespace <- o A..: "ns"
    pure $ CapturedLog
      { capturedLog_namespace = namespace
      , capturedLog_data  = dta
      , capturedLog_sev = sev
      , capturedLog_message = message
      }

-------------------------------------------------------------------------------
collectLogs :: KatipT IO () -> IO [CapturedLog]
collectLogs f = do
  capturedRef <- newIORef mempty
  withAsync (startServer capturedRef) $ \_worker -> do
    EX.bracket mkLE closeLE $ \le -> do
      runKatipT le f
  toList <$> readIORef capturedRef
  where
    closeLE = closeScribes
    port = 1337
    mkLE = do
      dataDogScribeSettings <- mkDataDogScribeSettings (localAgentConnectionParams (fromInteger port)) NoAuthLocal
      scribe <- mkDataDogScribe dataDogScribeSettings DebugS V3
      registerScribe "datadog" scribe defaultScribeSettings =<< initLogEnv "katip-datadog-tests" "test"
    startServer capturedRef = CN.runTCPServer (CN.serverSettings (fromInteger port) "*") $ \appData -> C.runConduit $
       CN.appSource appData C..|
         CB.lines C..|
         CL.map A.eitherDecodeStrict C..|
         CL.mapM_ (\parseResult -> case parseResult of
           Left e -> EX.throwM (ParseError e)
           (Right capturedLog) -> modifyIORef' capturedRef (\captured -> captured |> capturedLog))


-------------------------------------------------------------------------------
newtype ParseError = ParseError String
  deriving (Show)

instance EX.Exception ParseError

{-# LANGUAGE OverloadedStrings #-}
-- | Creates a scribe as a Custom Forwarder for Datadog that sends log
-- messages over TCP via their TLS endpoint.
module Katip.Scribes.Datadog.TCP
    ( -- * Types
      APIKey(..)
    , DataDogScribeSettings(..)
    , DataDogAuth(..)
    , directAPIConnectionParams
    , localAgentConnectionParams
    -- * Scribe construction
    , mkDataDogScribeSettings
    , mkDataDogScribe
    ) where



-------------------------------------------------------------------------------
import qualified Control.Concurrent     as Conc
import qualified Control.Exception.Safe as EX
import           Control.Monad          (void)
import qualified Control.Retry          as Retry
import qualified Data.Aeson             as A
import qualified Data.Binary.Builder    as BB
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Pool              as Pool
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as T
import qualified Data.Text.Lazy.Builder as B
import           Data.Time              (NominalDiffTime)
import           Katip
import           Katip.Core             (LocJs (..))
import qualified Network                as Net
import qualified Network.Connection     as C
import qualified System.Posix.Types     as POSIX
-------------------------------------------------------------------------------


-- | When writing directly to the main intake API, you must specify an
-- API key
newtype APIKey = APIKey
  { apiKey :: Text
  } deriving (Show, Eq)


data DataDogAuth =
    NoAuthLocal
  -- ^ Writing to the local agent, no auth required
  | DirectAuth APIKey
  -- ^ Writing directly to the main intake API over TLS . API Key is
  -- required. Using a local agent is recommended.


data DataDogScribeSettings = DataDogScribeSettings
  { dataDogScribeSettings_connectionParams    :: C.ConnectionParams
  -- ^ Specify where the logs should go. If writing directly to the
  -- main intake API, you can use 'directAPIConnectionParams'. If
  -- writing to a local agent, we recommend
  -- 'localAgentConnectionParams'
  , dataDogScribeSettings_auth                :: DataDogAuth
  , dataDogScribeSettings_poolStripes         :: Int
  -- ^ How many stripes should be used in the connection pool. 1 is a reasonable default
  , dataDogScribeSettings_connsPerStripe      :: Int
  -- ^ How many TCP connections per stripe should be maintained in the pool.
  , dataDogScribeSettings_connectionKeepalive :: NominalDiffTime
  -- ^ How long should a TCP connection be kept open. 30 (seconds) is a reasonable default. Anything around 60 will tend to time out a lot.
  , dataDogScribeSettings_retry               :: Retry.RetryPolicyM IO
  -- ^ How should exceptions during write be retried?
  }


-- | Configured for secure TLS communication to the main intake API at
-- intake.logs.datadoghq.com:10516. Must be combined with DirectAuth
-- authentication.
directAPIConnectionParams :: C.ConnectionParams
directAPIConnectionParams = C.ConnectionParams
  { C.connectionHostname = "intake.logs.datadoghq.com"
  , C.connectionPort = 10516
  , C.connectionUseSecure = Just $ C.TLSSettingsSimple
      { C.settingDisableCertificateValidation = False
      , C.settingDisableSession               = False
      , C.settingUseServerName                = False
      }
  , C.connectionUseSocks = Nothing
  }


-- | Configure to talk to a local DataDog agent at 127.0.0.1. TLS is not
-- utilized. This is the preferred type of connection because
-- authentication, buffering, and other more advanced features are
-- handled by the agent configuration.
localAgentConnectionParams :: Net.PortNumber -> C.ConnectionParams
localAgentConnectionParams port = C.ConnectionParams
  { C.connectionHostname = "127.0.0.1"
  , C.connectionPort = port
  , C.connectionUseSecure = Nothing
  , C.connectionUseSocks = Nothing
  }


-- | Reasonable defaults. Sets a 30s connection timeout, 1 stripe of
-- connections with a number of connections per stripe equal to
-- 'getNumCapabilities'. Retry policy will do an exponential backoff
-- with a 25ms base delay for up to 5 retries for a total cumulative delay of 775ms
mkDataDogScribeSettings :: C.ConnectionParams -> DataDogAuth -> IO DataDogScribeSettings
mkDataDogScribeSettings connectionParams auth = do
  capabilities <- Conc.getNumCapabilities
  pure $ DataDogScribeSettings
    { dataDogScribeSettings_connectionParams = connectionParams
    , dataDogScribeSettings_auth = auth
    , dataDogScribeSettings_poolStripes = 1
    , dataDogScribeSettings_connsPerStripe = capabilities
    , dataDogScribeSettings_connectionKeepalive = 30
    , dataDogScribeSettings_retry = Retry.exponentialBackoff 25000 <> Retry.limitRetries 5
    }


-------------------------------------------------------------------------------
mkDataDogScribe
  :: DataDogScribeSettings
  -> Severity
  -- ^ Severity level to observe
  -> Verbosity
  -- ^ Verbosity level to observe
  -> IO Scribe
mkDataDogScribe settings sev verb = do
  connectionContext <- C.initConnectionContext
  pool <- Pool.createPool
    (C.connectTo connectionContext (dataDogScribeSettings_connectionParams settings))
    C.connectionClose
    (dataDogScribeSettings_poolStripes settings)
    (dataDogScribeSettings_connectionKeepalive settings)
    (dataDogScribeSettings_connsPerStripe settings)
  pure $ Scribe
    { liPush = pushPool (dataDogScribeSettings_retry settings) pool keyBuilder verb
    , scribeFinalizer = Pool.destroyAllResources pool
    , scribePermitItem = permitItem sev
    }
  where
    !keyBuilder = case dataDogScribeSettings_auth settings of
      NoAuthLocal           -> Nothing
      DirectAuth (APIKey k) -> Just (BB.fromByteString (T.encodeUtf8 k))


pushPool
  :: LogItem a
  => Retry.RetryPolicyM IO
  -> Pool.Pool C.Connection
  -> Maybe BB.Builder
  -- ^ Rendered API key
  -> Verbosity
  -> Item a
  -> IO ()
pushPool retryPolicy pool token verb item =
  void $ Retry.retrying retryPolicy (\_status shouldRetry -> pure shouldRetry) $ \_status -> do
    res <- EX.tryAny $ Pool.withResource pool $ \conn -> do
      C.connectionPut conn rendered
    pure $ case res of
      Left _  -> True
      Right _ -> False
  where
    payloadBuilder = A.fromEncoding (encodeDatadog verb item) <> "\n"
    messageBuilder = case token of
      --TODO: lots of conversion going on here. could use a bench
      Nothing        -> payloadBuilder
      Just directKey -> directKey <> " " <> payloadBuilder
    rendered = BSL.toStrict (BB.toLazyByteString messageBuilder)


encodeDatadog :: LogItem a => Verbosity -> Item a -> A.Encoding
encodeDatadog verb i = A.pairs $
  -- datadog seems to use "service" to denote appname
  "service" A..= _itemApp i <>
  -- datadog uses severity (or some other alternatives for sev)
  "severity" A..= _itemSeverity i <>
  "env" A..= _itemEnv i <>
  "thread" A..= getThreadIdText (_itemThread i) <>
  "host" A..= _itemHost i <>
  "pid" A..= pidInt <>
  "data" A..= payloadObject verb (_itemPayload i) <>
  -- datadog uses "message" instead of msg
  "message" A..= (B.toLazyText (unLogStr (_itemMessage i))) <>
  -- datadog uses "date" instead of "at"
  "date" A..= _itemTime i <>
  "ns" A..= _itemNamespace i <>
  "loc" A..= fmap LocJs (_itemLoc i)
  where
    POSIX.CPid pidInt = _itemProcess i

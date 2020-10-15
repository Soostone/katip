{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- | Creates a scribe as a Custom Forwarder for Datadog that sends log
-- messages over TCP via their TLS endpoint.
module Katip.Scribes.Datadog.TCP
    ( -- * Types
      APIKey(..)
    , DatadogScribeSettings(..)
    , DatadogAuth(..)
    , directAPIConnectionParams
    , localAgentConnectionParams
    -- * Scribe construction
    , mkDatadogScribeSettings
    , mkDatadogScribe
    ) where



-------------------------------------------------------------------------------
import qualified Control.Concurrent     as Conc
import qualified Control.Exception.Safe as EX
import           Control.Monad          (void)
import qualified Control.Retry          as Retry
import qualified Data.Aeson             as A
import qualified Data.Binary.Builder    as BB
import qualified Data.ByteString.Lazy   as BSL
import           Data.Monoid            as Monoid
import qualified Data.Pool              as Pool
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as T
import qualified Data.Text.Lazy.Builder as B
import           Data.Time              (NominalDiffTime)
import           Katip
import           Katip.Core             (LocJs (..))
import qualified Network.Connection     as C
import qualified Network.Socket         as Net
import qualified System.Posix.Types     as POSIX
-------------------------------------------------------------------------------


-- | When writing directly to the main intake API, you must specify an
-- API key
newtype APIKey = APIKey
  { apiKey :: Text
  } deriving (Show, Eq)


data DatadogAuth =
    NoAuthLocal
  -- ^ Writing to the local agent, no auth required
  | DirectAuth APIKey
  -- ^ Writing directly to the main intake API over TLS . API Key is
  -- required. Using a local agent is recommended.


data DatadogScribeSettings = DatadogScribeSettings
  { datadogScribeSettings_connectionParams    :: C.ConnectionParams
  -- ^ Specify where the logs should go. If writing directly to the
  -- main intake API, you can use 'directAPIConnectionParams'. If
  -- writing to a local agent, we recommend
  -- 'localAgentConnectionParams'
  , datadogScribeSettings_auth                :: DatadogAuth
  , datadogScribeSettings_poolStripes         :: Int
  -- ^ How many stripes should be used in the connection pool. 1 is a reasonable default
  , datadogScribeSettings_connsPerStripe      :: Int
  -- ^ How many TCP connections per stripe should be maintained in the pool.
  , datadogScribeSettings_connectionKeepalive :: NominalDiffTime
  -- ^ How long should a TCP connection be kept open. 30 (seconds) is a reasonable default. Anything around 60 will tend to time out a lot.
  , datadogScribeSettings_retry               :: Retry.RetryPolicyM IO
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


-- | Configure to talk to a local Datadog agent at 127.0.0.1. TLS is not
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
mkDatadogScribeSettings :: C.ConnectionParams -> DatadogAuth -> IO DatadogScribeSettings
mkDatadogScribeSettings connectionParams auth = do
  capabilities <- Conc.getNumCapabilities
  pure $ DatadogScribeSettings
    { datadogScribeSettings_connectionParams = connectionParams
    , datadogScribeSettings_auth = auth
    , datadogScribeSettings_poolStripes = 1
    , datadogScribeSettings_connsPerStripe = capabilities
    , datadogScribeSettings_connectionKeepalive = 30
    , datadogScribeSettings_retry = Retry.exponentialBackoff 25000 <> Retry.limitRetries 5
    }


-------------------------------------------------------------------------------
mkDatadogScribe
  :: DatadogScribeSettings
  -> PermitFunc
  -- ^ Function on whether to permit items, e.g. @permitItem InfoS@
  -> Verbosity
  -- ^ Verbosity level to observe
  -> IO Scribe
mkDatadogScribe settings permit verb = do
  connectionContext <- C.initConnectionContext
  pool <- Pool.createPool
    (C.connectTo connectionContext (datadogScribeSettings_connectionParams settings))
    C.connectionClose
    (datadogScribeSettings_poolStripes settings)
    (datadogScribeSettings_connectionKeepalive settings)
    (datadogScribeSettings_connsPerStripe settings)
  pure $ Scribe
    { liPush = pushPool (datadogScribeSettings_retry settings) pool keyBuilder verb
    , scribeFinalizer = Pool.destroyAllResources pool
    , scribePermitItem = permit
    }
  where
    !keyBuilder = case datadogScribeSettings_auth settings of
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
    payloadBuilder = A.fromEncoding (encodeDatadog verb item) Monoid.<> "\n"
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

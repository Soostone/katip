{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Katip.Core where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.AutoUpdate
import           Control.Concurrent
import           Control.Lens
import           Control.Monad.Reader
import           Data.Aeson           (ToJSON)
import qualified Data.Aeson           as A
import qualified Data.Map.Strict      as M
import           Data.Monoid
import           Data.String
import           Data.Text            (Text)
import           Data.Time
import           GHC.Generics         hiding (to)
import           Network.HostName
-------------------------------------------------------------------------------


newtype AppName = AppName { getAppName :: Text }
  deriving (Eq,Show,Read,Ord,Generic,ToJSON,IsString)

newtype Environment = Environment { getEnvironment :: Text }
  deriving (Eq,Show,Read,Ord,Generic,ToJSON,IsString)


-------------------------------------------------------------------------------
data Severity
    = Debug                   -- ^ Debug messages
    | Info                    -- ^ Information
    | Notice                  -- ^ Normal runtime Conditions
    | Warning                 -- ^ General Warnings
    | Error                   -- ^ General Errors
    | Critical                -- ^ Severe situations
    | Alert                   -- ^ Take immediate action
    | Emergency               -- ^ System is unusable
  deriving (Eq, Ord, Show, Read, Generic)


-------------------------------------------------------------------------------
renderSeverity :: Severity -> Text
renderSeverity s = case s of
      Debug -> "Debug"
      Info -> "Debug"
      Notice -> "Notice"
      Warning -> "Warning"
      Error -> "Error"
      Critical -> "Critical"
      Alert -> "Alert"
      Emergency -> "Emergency"


-------------------------------------------------------------------------------
instance ToJSON Severity where
    toJSON s = A.String (renderSeverity s)


-------------------------------------------------------------------------------
-- | This has everything each log message will contain.
data Item a = Item {
      itemApp      :: AppName
    , itemEnv      :: Environment
    , itemSeverity :: Severity
    , itemThread   :: ThreadId
    , itemHost     :: HostName
    , itemPayload  :: a
    , itemMessage  :: Text
    , itemTime     :: UTCTime
    } deriving (Show,Generic)



instance ToJSON a => ToJSON (Item a) where
    toJSON Item{..} = A.object
      [ "app" A..= itemApp
      , "env" A..= itemEnv
      , "sev" A..= itemSeverity
      , "thread" A..= show itemThread
      , "host" A..= itemHost
      , "data" A..= itemPayload
      , "msg" A..= itemMessage
      , "at" A..= itemTime
      ]


-------------------------------------------------------------------------------
-- | Scribes are handlers of incoming items. Each registered scribe
-- knows how to push a log item somewhere.
data Scribe = Scribe {
      lhPush :: forall a. ToJSON a => Item a -> IO ()
    }


instance Monoid Scribe where
    mempty = Scribe $ const $ return ()
    mappend (Scribe a) (Scribe b) = Scribe $ \ item -> do
      a item
      b item

-------------------------------------------------------------------------------
data LogEnv = LogEnv {
      _logEnvHost     :: HostName
    , _logEnvApp      :: AppName
    , _logEnvEnv      :: Environment
    , _logEnvTimer    :: IO UTCTime
    , _logEnvHandlers :: M.Map Text Scribe
    }
makeLenses ''LogEnv


-------------------------------------------------------------------------------
initLogEnv :: AppName -> Environment -> IO LogEnv
initLogEnv an env = LogEnv
  <$> getHostName
  <*> pure an
  <*> pure env
  <*> mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime }
  <*> pure mempty


-------------------------------------------------------------------------------
registerHandler
    :: Text
    -- ^ Name the handler
    -> Scribe
    -> LogEnv
    -> LogEnv
registerHandler nm h = logEnvHandlers . at nm .~ Just h


-------------------------------------------------------------------------------
unregisterHandler
    :: Text
    -- ^ Name of the handler
    -> LogEnv
    -> LogEnv
unregisterHandler nm = logEnvHandlers . at nm .~ Nothing



class Katip m where
    getLogEnv :: m LogEnv



-------------------------------------------------------------------------------
-- | The generic *full* logging function.
logF
  :: (Applicative m, MonadIO m, ToJSON a, Katip m)
  => Severity
  -> a
  -- ^ Contextual payload for the log
  -> Text
  -- ^ The log message
  -> m ()
logF sev a msg = do
    LogEnv{..} <- getLogEnv
    item <- Item
      <$> pure _logEnvApp
      <*> pure _logEnvEnv
      <*> pure sev
      <*> liftIO myThreadId
      <*> pure _logEnvHost
      <*> pure a
      <*> pure msg
      <*> liftIO _logEnvTimer
    liftIO $ forM_ (M.elems _logEnvHandlers) $ \ (Scribe h) -> h item


-------------------------------------------------------------------------------
-- | Log a message without any context.
logM :: (Applicative m, MonadIO m, Katip m) => Severity -> Text -> m ()
logM sev msg = logF sev () msg


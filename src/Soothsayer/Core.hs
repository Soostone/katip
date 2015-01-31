{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Soothsayer.Core where

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
import           Data.Text            (Text)
import           Data.Time
import           GHC.Generics         hiding (to)
import           Network.HostName
-------------------------------------------------------------------------------


newtype AppName = AppName { getAppName :: Text }
  deriving (Eq,Show,Read,Ord,Generic,ToJSON)

newtype Environment = Environment { getEnvironment :: Text }
  deriving (Eq,Show,Read,Ord,Generic,ToJSON)


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
data LogHandler = LogHandler {
      lhPush :: forall a. ToJSON a => Item a -> IO ()
    }


-------------------------------------------------------------------------------
data LogEnv = LogEnv {
      _logEnvHost     :: HostName
    , _logEnvApp      :: AppName
    , _logEnvEnv      :: Environment
    , _logEnvTimer    :: IO UTCTime
    , _logEnvHandlers :: M.Map Text LogHandler
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
    -> LogHandler
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


-------------------------------------------------------------------------------
logM
  :: (Applicative m, MonadIO m, ToJSON a, MonadReader LogEnv m)
  => Severity
  -> a
  -> Text
  -> m ()
logM sev a msg = do
    item <- Item
      <$> view logEnvApp
      <*> view logEnvEnv
      <*> pure sev
      <*> liftIO myThreadId
      <*> view logEnvHost
      <*> pure a
      <*> pure msg
      <*> (view logEnvTimer >>= liftIO)
    hs <- view $ logEnvHandlers . to M.elems
    liftIO $ forM_ hs $ \ (LogHandler h) -> h item



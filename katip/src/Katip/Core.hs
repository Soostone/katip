{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
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
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Aeson                 (ToJSON (..))
import qualified Data.Aeson                 as A
import           Data.Foldable              (foldMap)
import qualified Data.HashMap.Strict        as HM
import           Data.List
import qualified Data.Map.Strict            as M
import           Data.Monoid
import           Data.String
import           Data.String.Conv
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy.Builder     as B
import           Data.Time
import           GHC.Generics               hiding (to)
import           Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax as TH
import           Network.HostName
import           System.Posix
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
newtype Namespace = Namespace { unNamespace :: [Text] }
  deriving (Eq,Show,Read,Ord,Generic,ToJSON,Monoid)

instance IsString Namespace where
    fromString s = Namespace [fromString s]


-------------------------------------------------------------------------------
-- | Ready namespace for emission with dots to join the segments.
intercalateNs :: Namespace -> [Text]
intercalateNs (Namespace xs) = intersperse "." xs


-------------------------------------------------------------------------------
-- | Application environment, like @prod@, @devel@, @testing@.
newtype Environment = Environment { getEnvironment :: Text }
  deriving (Eq,Show,Read,Ord,Generic,ToJSON,IsString)


-------------------------------------------------------------------------------
data Severity
    = DebugS                   -- ^ Debug messages
    | InfoS                    -- ^ Information
    | NoticeS                  -- ^ Normal runtime Conditions
    | WarningS                 -- ^ General Warnings
    | ErrorS                   -- ^ General Errors
    | CriticalS                -- ^ Severe situations
    | AlertS                   -- ^ Take immediate action
    | EmergencyS               -- ^ System is unusable
  deriving (Eq, Ord, Show, Read, Generic, Enum, Bounded)


-------------------------------------------------------------------------------
-- | Verbosity controls the amount of information (columns) a 'Scribe'
-- emits during logging.
--
-- The convention is:
-- - 'V0' implies no additional payload information is included in message.
-- - 'V3' implies the maximum amount of payload information.
-- - Anything in between is left to the discretion of the developer.
data Verbosity = V0 | V1 | V2 | V3
  deriving (Eq, Ord, Show, Read, Generic, Enum)


-------------------------------------------------------------------------------
renderSeverity :: Severity -> Text
renderSeverity s = case s of
      DebugS -> "Debug"
      InfoS -> "Info"
      NoticeS -> "Notice"
      WarningS -> "Warning"
      ErrorS -> "Error"
      CriticalS -> "Critical"
      AlertS -> "Alert"
      EmergencyS -> "Emergency"


-------------------------------------------------------------------------------
instance ToJSON Severity where
    toJSON s = A.String (renderSeverity s)


-------------------------------------------------------------------------------
-- | Log message with Builder unerneath; use '<>' to concat in O(1).
newtype LogStr = LogStr { unLogStr :: B.Builder }
    deriving (Generic)

instance IsString LogStr where
    fromString = LogStr . B.fromString

instance Monoid LogStr where
    mappend (LogStr a) (LogStr b) = LogStr (a `mappend` b)
    mempty = LogStr mempty

-------------------------------------------------------------------------------
-- | Pack any string-like thing into a 'LogStr'. This will
-- automatically work on 'String', 'ByteString, 'Text' and any of the
-- lazy variants.
logStr :: StringConv a Text => a -> LogStr
logStr t = LogStr (B.fromText $ toS t)


-------------------------------------------------------------------------------
-- | Shorthand for 'logMsg'
ls :: StringConv a Text => a -> LogStr
ls = logStr


-------------------------------------------------------------------------------
showLS :: Show a => a -> LogStr
showLS = ls . show


-------------------------------------------------------------------------------
-- | This has everything each log message will contain.
data Item a = Item {
      _itemApp       :: Namespace
    , _itemEnv       :: Environment
    , _itemSeverity  :: Severity
    , _itemThread    :: ThreadId
    , _itemHost      :: HostName
    , _itemProcess   :: ProcessID
    , _itemPayload   :: a
    , _itemMessage   :: LogStr
    , _itemTime      :: UTCTime
    , _itemNamespace :: Namespace
    , _itemLoc       :: Maybe Loc
    } deriving (Generic)
makeLenses ''Item


instance ToJSON a => ToJSON (Item a) where
    toJSON Item{..} = A.object
      [ "app" A..= _itemApp
      , "env" A..= _itemEnv
      , "sev" A..= _itemSeverity
      , "thread" A..= show _itemThread
      , "host" A..= _itemHost
      , "pid" A..= A.String (toS (show _itemProcess))
      , "data" A..= _itemPayload
      , "msg" A..= (B.toLazyText $ unLogStr _itemMessage)
      , "at" A..= _itemTime
      , "ns" A..= _itemNamespace
      , "loc" A..= fmap LocJs _itemLoc
      ]

newtype LocJs = LocJs { getLocJs :: Loc }


instance ToJSON LocJs where
    toJSON (LocJs (Loc fn p m (l, c) _)) = A.object
      [ "loc_fn" A..= fn
      , "loc_pkg" A..= p
      , "loc_mod" A..= m
      , "loc_ln" A..= l
      , "loc_col" A..= c
      ]


-------------------------------------------------------------------------------
-- | Field selector by verbosity within JSON payload.
data PayloadSelection
    = AllKeys
    | SomeKeys [Text]

instance Monoid PayloadSelection where
    mempty = SomeKeys []
    mappend AllKeys _ = AllKeys
    mappend _ AllKeys = AllKeys
    mappend (SomeKeys as) (SomeKeys bs) = SomeKeys (as++bs)


-------------------------------------------------------------------------------
-- | Katip requires JSON objects to be logged as context. This
-- typeclass provides a default instance which uses ToJSON and
-- produces an empty object if 'toJSON' results in any type other than
-- object. If you have a type you want to log that produces an Array
-- or Number for example, you'll want to write an explicit instance
-- here. You can trivially add a ToObject instance for something with
-- a ToJSON instance like:
--
-- > instance ToObject Foo
class ToJSON a => ToObject a where
    toObject :: a -> A.Object
    toObject v = case toJSON v of
      A.Object o -> o
      _        -> mempty

instance ToObject ()
instance ToObject A.Object

-------------------------------------------------------------------------------
-- | Payload objects need instances of this class.
--
-- When defining 'payloadKeys', don't redundantly declare the same
-- keys for higher levels of verbosity. Each level of verbosity
-- automatically and recursively contains all keys from the level
-- before it.
class ToObject a => LogItem a where

    -- | List of keys in the JSON object that should be included in message.
    payloadKeys :: Verbosity -> a -> PayloadSelection


instance LogItem () where payloadKeys _ _ = SomeKeys []


-------------------------------------------------------------------------------
-- | Constrain payload based on verbosity. Backends should use this to
-- automatically bubble higher verbosity levels to lower ones.
payloadObject :: LogItem a => Verbosity -> a -> A.Object
payloadObject verb a = case foldMap (flip payloadKeys a) [(V0)..verb] of
    AllKeys -> toObject a
    SomeKeys ks -> HM.filterWithKey (\ k _ -> k `elem` ks) $ toObject a


-------------------------------------------------------------------------------
-- | Convert log item to its JSON representation while trimming its
-- payload based on the desired verbosity. Backends that push JSON
-- messages should use this to obtain their payload.
itemJson :: LogItem a => Verbosity -> Item a -> A.Value
itemJson verb a = toJSON $ a & itemPayload %~ payloadObject verb


-------------------------------------------------------------------------------
-- | Scribes are handlers of incoming items. Each registered scribe
-- knows how to push a log item somewhere.
data Scribe = Scribe {
      lhPush :: forall a. LogItem a => Item a -> IO ()
    }


instance Monoid Scribe where
    mempty = Scribe $ const $ return ()
    mappend (Scribe a) (Scribe b) = Scribe $ \ item -> do
      a item
      b item

-------------------------------------------------------------------------------
data LogEnv = LogEnv {
      _logEnvHost    :: HostName
    , _logEnvPid     :: ProcessID
    , _logEnvNs      :: Namespace
    , _logEnvEnv     :: Environment
    , _logEnvTimer   :: IO UTCTime
    , _logEnvScribes :: M.Map Text Scribe
    }
makeLenses ''LogEnv


-------------------------------------------------------------------------------
initLogEnv
    :: Namespace
    -- ^ A base namespace for this application
    -> Environment
    -- ^ Current run environment (e.g. @prod@ vs. @devel@)
    -> IO LogEnv
initLogEnv an env = LogEnv
  <$> getHostName
  <*> getProcessID
  <*> pure an
  <*> pure env
  <*> mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime }
  <*> pure mempty


-------------------------------------------------------------------------------
registerScribe
    :: Text
    -- ^ Name the scribe
    -> Scribe
    -> LogEnv
    -> LogEnv
registerScribe nm h = logEnvScribes . at nm .~ Just h


-------------------------------------------------------------------------------
unregisterScribe
    :: Text
    -- ^ Name of the scribe
    -> LogEnv
    -> LogEnv
unregisterScribe nm = logEnvScribes . at nm .~ Nothing



-------------------------------------------------------------------------------
-- | Monads where katip logging actions can be performed
class MonadIO m =>  Katip m where
    getLogEnv :: m LogEnv


instance Katip m => Katip (ReaderT s m) where
    getLogEnv = lift getLogEnv


instance Katip m => Katip (EitherT s m) where
    getLogEnv = lift getLogEnv


instance Katip m => Katip (MaybeT m) where
    getLogEnv = lift getLogEnv


instance Katip m => Katip (StateT s m) where
    getLogEnv = lift getLogEnv


instance (Katip m, Monoid s) => Katip (WriterT s m) where
    getLogEnv = lift getLogEnv


-------------------------------------------------------------------------------
-- | A concrete monad you can use to run logging actions.
newtype KatipT m a = KatipT { unKatipT :: ReaderT LogEnv m a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadMask, MonadCatch, MonadThrow, MonadTrans )


instance MonadIO m => Katip (KatipT m) where
    getLogEnv = KatipT ask


-------------------------------------------------------------------------------
-- | Execute 'KatipT' on a log env.
runKatipT :: LogEnv -> KatipT m a -> m a
runKatipT le (KatipT f) = runReaderT f le


-------------------------------------------------------------------------------
-- | Log with everything, including a source code location. This is
-- very low level and you typically can use 'logT' in its place.
logItem
    :: (Applicative m, LogItem a, Katip m)
    => a
    -> Namespace
    -> Maybe Loc
    -> Severity
    -> LogStr
    -> m ()
logItem a ns loc sev msg = do
    LogEnv{..} <- getLogEnv
    item <- Item
      <$> pure _logEnvNs
      <*> pure _logEnvEnv
      <*> pure sev
      <*> liftIO myThreadId
      <*> pure _logEnvHost
      <*> pure _logEnvPid
      <*> pure a
      <*> pure msg
      <*> liftIO _logEnvTimer
      <*> pure (_logEnvNs <> ns)
      <*> pure loc
    liftIO $ forM_ (M.elems _logEnvScribes) $ \ (Scribe h) -> h item


-------------------------------------------------------------------------------
-- | Log with full context, but without any code location.
logF
  :: (Applicative m, LogItem a, Katip m)
  => a
  -- ^ Contextual payload for the log
  -> Namespace
  -- ^ Specific namespace of the message.
  -> Severity
  -- ^ Severity of the message
  -> LogStr
  -- ^ The log message
  -> m ()
logF a ns sev msg = logItem a ns Nothing sev msg


-------------------------------------------------------------------------------
-- | Log a message without any payload/context or code location.
logMsg
    :: (Applicative m, Katip m)
    => Namespace
    -> Severity
    -> LogStr
    -> m ()
logMsg ns sev msg = logF () ns sev msg


instance TH.Lift Namespace where
    lift (Namespace xs) =
      let xs' = map T.unpack xs
      in  [| Namespace (map T.pack xs') |]


instance TH.Lift Verbosity where
    lift V0 = [| V0 |]
    lift V1 = [| V1 |]
    lift V2 = [| V2 |]
    lift V3 = [| V3 |]


instance TH.Lift Severity where
    lift DebugS = [| DebugS |]
    lift InfoS  = [| InfoS |]
    lift NoticeS  = [| NoticeS |]
    lift WarningS  = [| WarningS |]
    lift ErrorS  = [| ErrorS |]
    lift CriticalS  = [| CriticalS |]
    lift AlertS  = [| AlertS |]
    lift EmergencyS  = [| EmergencyS |]


-- | Lift a location into an Exp.
liftLoc :: Loc -> Q Exp
liftLoc (Loc a b c (d1, d2) (e1, e2)) = [|Loc
    $(TH.lift a)
    $(TH.lift b)
    $(TH.lift c)
    ($(TH.lift d1), $(TH.lift d2))
    ($(TH.lift e1), $(TH.lift e2))
    |]


-------------------------------------------------------------------------------
-- | For use when you want to include location in your logs. This will
-- fill the 'Maybe Loc' gap in 'logF' of this module.
getLoc :: Q Exp
getLoc = [| $(location >>= liftLoc) |]


-------------------------------------------------------------------------------
-- | 'Loc'-tagged logging when using template-haskell is OK.
--
-- @$(logT) obj mempty InfoS "Hello world"@
logT :: ExpQ
logT = [| \ a ns sev msg -> logItem a ns (Just $(getLoc)) sev msg |]


-- taken from the file-location package
-- turn the TH Loc loaction information into a human readable string
-- leaving out the loc_end parameter
locationToString :: Loc -> String
locationToString loc = (loc_package loc) ++ ':' : (loc_module loc) ++
  ' ' : (loc_filename loc) ++ ':' : (line loc) ++ ':' : (char loc)
  where
    line = show . fst . loc_start
    char = show . snd . loc_start
